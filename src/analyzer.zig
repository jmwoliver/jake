const std = @import("std");
const ast = @import("ast.zig");

/// Error details from argument validation
pub const ValidationError = struct {
    kind: Kind,
    param_name: []const u8,
    value: ?[]const u8 = null,
    options: ?[]const []const u8 = null,

    pub const Kind = enum {
        missing_required_param,
        invalid_param_value,
        too_many_arguments,
        unknown_parameter,
        placeholder_no_default,
    };
};

pub const AnalyzerError = struct {
    kind: Kind,
    loc: ast.Location,
    message: []const u8,
    hint: ?[]const u8 = null,

    pub const Kind = enum {
        undefined_function,
        missing_required_param,
        invalid_param_value,
        too_many_arguments,
        unknown_parameter,
        default_not_in_opts,
        // Match statement errors
        match_on_unconstrained,
        invalid_match_value,
        duplicate_match_value,
        non_exhaustive_match,
        undefined_variable,
    };
};

pub const AnalysisResult = struct {
    errors: []const AnalyzerError,
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *AnalysisResult) void {
        self.arena.deinit(); // Frees all diagnostic strings
        self.allocator.free(self.errors);
    }

    pub fn hasErrors(self: *const AnalysisResult) bool {
        return self.errors.len > 0;
    }
};

pub const Analyzer = struct {
    allocator: std.mem.Allocator,
    parsed_ast: *const ast.Ast,
    errors: std.array_list.Managed(AnalyzerError),
    string_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, parsed_ast: *const ast.Ast) Analyzer {
        return .{
            .allocator = allocator,
            .parsed_ast = parsed_ast,
            .errors = std.array_list.Managed(AnalyzerError).init(allocator),
            .string_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn analyze(allocator: std.mem.Allocator, parsed_ast: *const ast.Ast) AnalysisResult {
        var analyzer = Analyzer.init(allocator, parsed_ast);

        // Validate all function definitions
        var iter = parsed_ast.functions.valueIterator();
        while (iter.next()) |func| {
            analyzer.validateFunction(func);
        }

        return .{
            .errors = analyzer.errors.toOwnedSlice() catch &[_]AnalyzerError{},
            .allocator = allocator,
            .arena = analyzer.string_arena,
        };
    }

    fn validateFunction(self: *Analyzer, func: *const ast.FunctionDef) void {
        // Validate parameter specs
        for (func.params) |param| {
            self.validateParamSpec(&param);
        }

        // Validate function body
        self.validateStatements(func.body, func);
    }

    fn validateStatements(self: *Analyzer, statements: []const ast.Statement, func: *const ast.FunctionDef) void {
        for (statements) |stmt| {
            switch (stmt) {
                .shell_command => {}, // Shell commands are always valid
                .jake_call => |call| self.validateJakeCall(&call),
                .match_statement => |match_stmt| {
                    self.validateMatchStatement(&match_stmt, func);
                },
                .var_assignment => {}, // Variable assignments are always valid
            }
        }
    }

    fn validateMatchStatement(
        self: *Analyzer,
        match_stmt: *const ast.MatchStatement,
        func: *const ast.FunctionDef,
    ) void {
        // 1. Find the parameter being matched
        var param: ?*const ast.ParamSpec = null;
        for (func.params) |*p| {
            if (std.mem.eql(u8, p.name, match_stmt.variable)) {
                param = p;
                break;
            }
        }

        if (param == null) {
            self.addError(
                .undefined_variable,
                match_stmt.variable_loc,
                "undefined variable in match",
                null,
            );
            return;
        }

        // 2. Verify param has constrained options
        const options = param.?.options orelse {
            self.addError(
                .match_on_unconstrained,
                match_stmt.variable_loc,
                "match can only be used on parameters with constrained values",
                std.fmt.allocPrint(self.string_arena.allocator(), "add constraints like '{s}: {{opt1|opt2}}' to enable match", .{match_stmt.variable}) catch null,
            );
            return;
        };

        // 3. Check exhaustiveness and validate values
        var covered = std.StringHashMap(bool).init(self.allocator);
        defer covered.deinit();

        for (match_stmt.arms) |arm| {
            for (arm.values) |value| {
                // Check value is valid option
                var is_valid = false;
                for (options) |opt| {
                    if (std.mem.eql(u8, opt, value)) {
                        is_valid = true;
                        break;
                    }
                }
                if (!is_valid) {
                    // Build valid options string for hint
                    var opts_list = std.array_list.Managed(u8).init(self.string_arena.allocator());
                    for (options, 0..) |opt, i| {
                        opts_list.appendSlice(opt) catch {};
                        if (i < options.len - 1) {
                            opts_list.appendSlice(", ") catch {};
                        }
                    }
                    const opts_str = opts_list.toOwnedSlice() catch null;

                    self.addError(
                        .invalid_match_value,
                        match_stmt.variable_loc,
                        std.fmt.allocPrint(self.string_arena.allocator(), "'{s}' is not a valid option for parameter '{s}'", .{ value, match_stmt.variable }) catch "invalid match value",
                        if (opts_str) |s| std.fmt.allocPrint(self.string_arena.allocator(), "valid options are: {s}", .{s}) catch null else null,
                    );
                }

                // Check for duplicates
                if (covered.contains(value)) {
                    self.addError(
                        .duplicate_match_value,
                        match_stmt.variable_loc,
                        std.fmt.allocPrint(self.string_arena.allocator(), "'{s}' appears in multiple match arms", .{value}) catch "duplicate match value",
                        null,
                    );
                } else {
                    covered.put(value, true) catch {};
                }
            }

            // Recursively validate arm body
            self.validateStatements(arm.body, func);
        }

        // Check all options are covered
        var missing = std.array_list.Managed([]const u8).init(self.allocator);
        defer missing.deinit();

        for (options) |opt| {
            if (!covered.contains(opt)) {
                missing.append(opt) catch {};
            }
        }

        if (missing.items.len > 0) {
            // Build missing values string
            var missing_str = std.array_list.Managed(u8).init(self.string_arena.allocator());
            for (missing.items, 0..) |m, i| {
                missing_str.appendSlice(m) catch {};
                if (i < missing.items.len - 1) {
                    missing_str.appendSlice(", ") catch {};
                }
            }

            self.addError(
                .non_exhaustive_match,
                match_stmt.variable_loc,
                "match is not exhaustive",
                std.fmt.allocPrint(self.string_arena.allocator(), "missing values: {s}", .{missing_str.toOwnedSlice() catch "?"}) catch null,
            );
        }
    }

    fn validateParamSpec(self: *Analyzer, param: *const ast.ParamSpec) void {
        // If both default and opts are set, default must be in opts
        if (param.default) |default| {
            if (param.options) |opts| {
                var found = false;
                for (opts) |opt| {
                    if (std.mem.eql(u8, opt, default)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    self.addError(.default_not_in_opts, .{
                        .line = 0,
                        .column = 0,
                        .start = 0,
                        .end = 0,
                    }, "default value must be one of the allowed options", null);
                }
            }
        }
    }

    fn validateJakeCall(self: *Analyzer, call: *const ast.JakeCall) void {
        // Check if function exists
        if (!self.parsed_ast.functions.contains(call.name)) {
            // Try to find a similar function name for hint
            var hint: ?[]const u8 = null;
            var iter = self.parsed_ast.functions.keyIterator();
            while (iter.next()) |key| {
                if (similarEnough(call.name, key.*)) {
                    hint = key.*;
                    break;
                }
            }

            const hint_msg = if (hint) |h|
                std.fmt.allocPrint(self.string_arena.allocator(), "did you mean '{s}'?", .{h}) catch null
            else
                null;

            self.addError(.undefined_function, call.name_loc, "undefined function", hint_msg);
        }
    }

    fn addError(self: *Analyzer, kind: AnalyzerError.Kind, loc: ast.Location, message: []const u8, hint: ?[]const u8) void {
        self.errors.append(.{
            .kind = kind,
            .loc = loc,
            .message = message,
            .hint = hint,
        }) catch {};
    }
};

/// Check if two strings are similar enough to suggest as alternative
fn similarEnough(a: []const u8, b: []const u8) bool {
    // Simple check: same prefix of 2+ chars
    if (a.len < 2 or b.len < 2) return false;
    if (a[0] == b[0] and a[1] == b[1]) return true;

    // Check if one is substring of other
    if (std.mem.indexOf(u8, a, b) != null) return true;
    if (std.mem.indexOf(u8, b, a) != null) return true;

    return false;
}

/// Result of argument validation - either success with resolved args or detailed error
pub const ValidationResult = union(enum) {
    ok: std.StringHashMap([]const u8),
    err: ValidationError,

    pub fn deinit(self: *ValidationResult) void {
        switch (self.*) {
            .ok => |*map| map.deinit(),
            .err => {},
        }
    }
};

/// Validate CLI arguments against a function's parameter specs
pub fn validateArguments(
    allocator: std.mem.Allocator,
    func: *const ast.FunctionDef,
    cli_args: []const ast.Argument,
) error{OutOfMemory}!ValidationResult {
    var resolved = std.StringHashMap([]const u8).init(allocator);
    errdefer resolved.deinit();

    var positional_idx: usize = 0;

    // First pass: collect named arguments
    for (cli_args) |arg| {
        if (arg.name) |name| {
            // Validate the parameter exists
            var found = false;
            for (func.params) |param| {
                if (std.mem.eql(u8, param.name, name)) {
                    found = true;

                    // Handle placeholder (arch=_)
                    if (arg.use_default) {
                        if (param.default) |default| {
                            try resolved.put(name, default);
                        } else {
                            resolved.deinit();
                            return .{ .err = .{
                                .kind = .placeholder_no_default,
                                .param_name = name,
                            } };
                        }
                        break;
                    }

                    // Validate value against options if specified
                    if (param.options) |opts| {
                        var valid = false;
                        for (opts) |opt| {
                            if (std.mem.eql(u8, opt, arg.value)) {
                                valid = true;
                                break;
                            }
                        }
                        if (!valid) {
                            resolved.deinit();
                            return .{ .err = .{
                                .kind = .invalid_param_value,
                                .param_name = name,
                                .value = arg.value,
                                .options = opts,
                            } };
                        }
                    }
                    try resolved.put(name, arg.value);
                    break;
                }
            }
            if (!found) {
                resolved.deinit();
                return .{ .err = .{
                    .kind = .unknown_parameter,
                    .param_name = name,
                } };
            }
        }
    }

    // Second pass: assign positional arguments
    for (cli_args) |arg| {
        if (arg.name == null) {
            // Find next unassigned parameter
            while (positional_idx < func.params.len) {
                const param = &func.params[positional_idx];
                if (!resolved.contains(param.name)) {
                    // Handle placeholder (_)
                    if (arg.use_default) {
                        if (param.default) |default| {
                            try resolved.put(param.name, default);
                            positional_idx += 1;
                            break;
                        } else {
                            resolved.deinit();
                            return .{ .err = .{
                                .kind = .placeholder_no_default,
                                .param_name = param.name,
                            } };
                        }
                    }

                    // Validate value against options if specified
                    if (param.options) |opts| {
                        var valid = false;
                        for (opts) |opt| {
                            if (std.mem.eql(u8, opt, arg.value)) {
                                valid = true;
                                break;
                            }
                        }
                        if (!valid) {
                            resolved.deinit();
                            return .{ .err = .{
                                .kind = .invalid_param_value,
                                .param_name = param.name,
                                .value = arg.value,
                                .options = opts,
                            } };
                        }
                    }
                    try resolved.put(param.name, arg.value);
                    positional_idx += 1;
                    break;
                }
                positional_idx += 1;
            } else {
                resolved.deinit();
                return .{ .err = .{
                    .kind = .too_many_arguments,
                    .param_name = "",
                    .value = arg.value,
                } };
            }
        }
    }

    // Third pass: apply defaults and check required params
    for (func.params) |param| {
        if (!resolved.contains(param.name)) {
            if (param.default) |default| {
                try resolved.put(param.name, default);
            } else {
                resolved.deinit();
                return .{ .err = .{
                    .kind = .missing_required_param,
                    .param_name = param.name,
                    .options = param.options,
                } };
            }
        }
    }

    return .{ .ok = resolved };
}

// Tests
test "analyzer detects undefined function" {
    const source = "test() { @undefined_func() }";
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(AnalyzerError.Kind.undefined_function, result.errors[0].kind);
}

test "analyzer allows shell commands" {
    const source = "test() { echo hello }";
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
}

test "analyzer allows defined function calls" {
    const source =
        \\build() { echo build }
        \\test() { @build() }
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
}

test "validateArguments resolves positional args" {
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = null, .options = null },
        .{ .name = "num", .default = null, .options = null },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "arm64" },
        .{ .value = "5" },
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("arm64", resolved.get("arch").?);
            try std.testing.expectEqualStrings("5", resolved.get("num").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "validateArguments applies defaults" {
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = "arm64", .options = null },
    };
    const func = ast.FunctionDef{
        .name = "build",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 5 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    var result = try validateArguments(std.testing.allocator, &func, &[_]ast.Argument{});
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("arm64", resolved.get("arch").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "validateArguments validates options" {
    const opts = [_][]const u8{ "single", "lb", "k8s" };
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = null, .options = &opts },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    // Valid option
    const valid_args = [_]ast.Argument{.{ .value = "lb" }};
    var result = try validateArguments(std.testing.allocator, &func, &valid_args);
    result.deinit();
    try std.testing.expect(result == .ok);

    // Invalid option
    const invalid_args = [_]ast.Argument{.{ .value = "invalid" }};
    var invalid_result = try validateArguments(std.testing.allocator, &func, &invalid_args);
    defer invalid_result.deinit();
    switch (invalid_result) {
        .err => |ve| try std.testing.expectEqual(ValidationError.Kind.invalid_param_value, ve.kind),
        .ok => try std.testing.expect(false),
    }
}

test "validateArguments handles named args" {
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = null, .options = null },
        .{ .name = "num", .default = null, .options = null },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "5", .name = "num" },
        .{ .value = "arm64", .name = "arch" },
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("arm64", resolved.get("arch").?);
            try std.testing.expectEqualStrings("5", resolved.get("num").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "validateArguments resolves underscore placeholder to default" {
    const opts = [_][]const u8{ "arm64", "x86_64" };
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = "arm64", .options = &opts },
        .{ .name = "num", .default = null, .options = null },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "", .use_default = true }, // _ placeholder
        .{ .value = "5" },
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("arm64", resolved.get("arch").?);
            try std.testing.expectEqualStrings("5", resolved.get("num").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "validateArguments errors on underscore without default" {
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = null, .options = null }, // No default!
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "", .use_default = true }, // _ placeholder
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .err => |ve| try std.testing.expectEqual(ValidationError.Kind.placeholder_no_default, ve.kind),
        .ok => try std.testing.expect(false),
    }
}

test "validateArguments handles multiple underscores" {
    const params = [_]ast.ParamSpec{
        .{ .name = "a", .default = "default_a", .options = null },
        .{ .name = "b", .default = "default_b", .options = null },
        .{ .name = "c", .default = null, .options = null },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "", .use_default = true }, // _
        .{ .value = "", .use_default = true }, // _
        .{ .value = "explicit" },
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("default_a", resolved.get("a").?);
            try std.testing.expectEqualStrings("default_b", resolved.get("b").?);
            try std.testing.expectEqualStrings("explicit", resolved.get("c").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "validateArguments handles named underscore placeholder" {
    const params = [_]ast.ParamSpec{
        .{ .name = "arch", .default = "arm64", .options = null },
    };
    const func = ast.FunctionDef{
        .name = "test",
        .name_loc = .{ .line = 1, .column = 1, .start = 0, .end = 4 },
        .params = &params,
        .body = &[_]ast.Statement{},
    };

    const args = [_]ast.Argument{
        .{ .value = "", .name = "arch", .use_default = true }, // arch=_
    };

    var result = try validateArguments(std.testing.allocator, &func, &args);
    defer result.deinit();

    switch (result) {
        .ok => |resolved| {
            try std.testing.expectEqualStrings("arm64", resolved.get("arch").?);
        },
        .err => try std.testing.expect(false),
    }
}

test "analyzer validates exhaustive match" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      echo debug
        \\    }
        \\    release: {
        \\      echo release
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
}

test "analyzer detects non-exhaustive match" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      echo debug
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(AnalyzerError.Kind.non_exhaustive_match, result.errors[0].kind);
}

test "analyzer detects match on unconstrained param" {
    const source =
        \\build(name) {
        \\  match $name {
        \\    foo: {
        \\      echo foo
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(AnalyzerError.Kind.match_on_unconstrained, result.errors[0].kind);
}

test "analyzer detects invalid match value" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      echo debug
        \\    }
        \\    fast: {
        \\      echo fast
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    // Should have both invalid_match_value and non_exhaustive_match
    var has_invalid = false;
    var has_non_exhaustive = false;
    for (result.errors) |err| {
        if (err.kind == .invalid_match_value) has_invalid = true;
        if (err.kind == .non_exhaustive_match) has_non_exhaustive = true;
    }
    try std.testing.expect(has_invalid);
    try std.testing.expect(has_non_exhaustive);
}

test "analyzer detects duplicate match value" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      echo debug1
        \\    }
        \\    debug: {
        \\      echo debug2
        \\    }
        \\    release: {
        \\      echo release
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(AnalyzerError.Kind.duplicate_match_value, result.errors[0].kind);
}

test "analyzer validates multi-value match arms" {
    const source =
        \\build(type: {debug|release|small}) {
        \\  match $type {
        \\    debug: {
        \\      echo debug
        \\    }
        \\    release | small: {
        \\      echo optimized
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
}

test "analyzer detects undefined variable in match" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $undefined {
        \\    debug: {
        \\      echo debug
        \\    }
        \\  }
        \\}
    ;
    var parsed = @import("parser.zig").Parser.parse(std.testing.allocator, source);
    defer parsed.deinit();

    var result = Analyzer.analyze(std.testing.allocator, &parsed);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(AnalyzerError.Kind.undefined_variable, result.errors[0].kind);
}
