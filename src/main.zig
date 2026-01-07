const std = @import("std");
const parser = @import("parser.zig");
const analyzer = @import("analyzer.zig");
const executor = @import("executor.zig");
const diagnostics = @import("diagnostics.zig");
const ast = @import("ast.zig");

/// Initial config from CLI flags (before we know function definitions)
const PreConfig = struct {
    jakefile: []const u8,
    show_help: bool,
    raw_args: []const []const u8, // Remaining non-flag arguments
};

/// Fully resolved config with target chain
const Config = struct {
    targets: std.array_list.Managed(ast.TargetInvocation),
    allocator: std.mem.Allocator,

    fn deinit(self: *Config) void {
        for (self.targets.items) |target| {
            self.allocator.free(target.args);
        }
        self.targets.deinit();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const stderr_file = std.fs.File.stderr();
    const stdout_file = std.fs.File.stdout();
    const stderr = stderr_file.deprecatedWriter();
    const stdout = stdout_file.deprecatedWriter();
    const use_color = stderr_file.getOrEnableAnsiEscapeSupport();

    // First pass: extract flags and collect raw args
    const pre_config = parsePreConfig(args[1..]) catch |err| {
        switch (err) {
            error.MissingJakefilePath => {
                try diagnostics.printExecutionError(stderr, "argument", "-f requires a file path", use_color);
            },
        }
        std.process.exit(1);
    };

    if (pre_config.show_help) {
        try diagnostics.printHelp(stdout, use_color);
        return;
    }

    // Find and load Jakefile
    const jakefile_path = pre_config.jakefile;
    const source = loadJakefile(allocator, jakefile_path) catch |err| {
        switch (err) {
            error.FileNotFound => {
                try diagnostics.printExecutionError(stderr, "file", "Jakefile not found", use_color);
            },
            else => {
                try diagnostics.printExecutionError(stderr, "file", "failed to read Jakefile", use_color);
            },
        }
        std.process.exit(1);
    };
    defer allocator.free(source);

    // Parse
    var parsed = parser.Parser.parse(allocator, source);
    defer parsed.deinit();

    if (parsed.hasErrors()) {
        for (parsed.errors.items) |err| {
            try diagnostics.printParseError(stderr, err, source, jakefile_path, use_color);
        }
        std.process.exit(1);
    }

    // Analyze
    var analysis = analyzer.Analyzer.analyze(allocator, &parsed);
    defer analysis.deinit();

    if (analysis.hasErrors()) {
        for (analysis.errors) |err| {
            try diagnostics.printAnalyzerError(stderr, err, source, jakefile_path, use_color);
        }
        std.process.exit(1);
    }

    // No args specified - list available targets
    if (pre_config.raw_args.len == 0) {
        try diagnostics.printTargetList(stdout, &parsed.functions, use_color);
        return;
    }

    // Second pass: resolve raw args into target chain using function definitions
    var config = resolveTargetChain(allocator, pre_config.raw_args, &parsed.functions) catch |err| {
        switch (err) {
            error.UnknownTarget => |_| {
                // Error already printed
            },
            error.OutOfMemory => {
                try diagnostics.printExecutionError(stderr, "memory", "out of memory", use_color);
            },
        }
        std.process.exit(1);
    };
    defer config.deinit();

    // Execute target chain
    var exec = executor.Executor.init(allocator, &parsed.functions, source);
    const result = exec.executeChain(config.targets.items);

    switch (result) {
        .ok => {},
        .validation_err => |ve| {
            try diagnostics.printChainValidationError(stderr, ve, use_color);
            std.process.exit(1);
        },
        .err => |err| {
            switch (err.err) {
                error.UndefinedFunction => {
                    try diagnostics.printExecutionError(stderr, "undefined function", err.target_name, use_color);
                },
                error.UndefinedVariable => {
                    try diagnostics.printExecutionError(stderr, "undefined variable", "variable not defined in scope", use_color);
                },
                error.CommandFailed => {
                    // Error message already printed by executor
                },
                error.OutOfMemory => {
                    try diagnostics.printExecutionError(stderr, "memory", "out of memory", use_color);
                },
            }
            std.process.exit(1);
        },
    }
}

/// First pass: extract CLI flags and collect remaining args
fn parsePreConfig(args: []const []const u8) error{MissingJakefilePath}!PreConfig {
    var config = PreConfig{
        .jakefile = "Jakefile",
        .show_help = false,
        .raw_args = &[_][]const u8{},
    };

    var raw_start: usize = 0;
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            config.show_help = true;
            return config;
        }

        if (std.mem.eql(u8, arg, "-f")) {
            i += 1;
            if (i >= args.len) return error.MissingJakefilePath;
            config.jakefile = args[i];
            raw_start = i + 1;
            continue;
        }

        if (arg.len > 0 and arg[0] == '-') {
            // Unknown flag, skip for now
            raw_start = i + 1;
            continue;
        }

        // First non-flag arg - rest are raw args
        config.raw_args = args[i..];
        break;
    }

    return config;
}

/// Second pass: resolve raw CLI args into target chain using function definitions
/// This is the "just-style" parsing that looks ahead at function param counts
fn resolveTargetChain(
    allocator: std.mem.Allocator,
    raw_args: []const []const u8,
    functions: *const std.StringHashMap(ast.FunctionDef),
) error{ UnknownTarget, OutOfMemory }!Config {
    var config = Config{
        .targets = std.array_list.Managed(ast.TargetInvocation).init(allocator),
        .allocator = allocator,
    };
    errdefer config.deinit();

    var i: usize = 0;
    while (i < raw_args.len) {
        const target_name = raw_args[i];

        // Look up the function to know how many params it takes
        const func = functions.get(target_name) orelse {
            // Not a known target - print error
            const stderr = std.fs.File.stderr().deprecatedWriter();
            const use_color = std.fs.File.stderr().getOrEnableAnsiEscapeSupport();
            const reset = if (use_color) "\x1b[0m" else "";
            const bold_red = if (use_color) "\x1b[1;31m" else "";
            stderr.print("{s}error{s}: unknown target '{s}'\n", .{ bold_red, reset, target_name }) catch {};
            return error.UnknownTarget;
        };

        i += 1; // Move past target name

        // Collect arguments for this target based on param count
        var target_args = std.array_list.Managed(ast.Argument).init(allocator);
        errdefer target_args.deinit();

        const param_count = func.params.len;
        var args_consumed: usize = 0;

        while (args_consumed < param_count and i < raw_args.len) {
            const arg = raw_args[i];

            // Check if this arg is actually another target (stop consuming)
            if (functions.contains(arg)) {
                break;
            }

            try target_args.append(parseArgument(arg));
            args_consumed += 1;
            i += 1;
        }

        try config.targets.append(.{
            .name = target_name,
            .args = try target_args.toOwnedSlice(),
        });
    }

    return config;
}

fn parseArgument(arg: []const u8) ast.Argument {
    // Check for named argument (name=value)
    if (std.mem.indexOf(u8, arg, "=")) |eq_pos| {
        const name = arg[0..eq_pos];
        const value = arg[eq_pos + 1 ..];

        // Handle named argument with underscore placeholder (arch=_)
        if (std.mem.eql(u8, value, "_")) {
            return ast.Argument{
                .name = name,
                .value = "",
                .use_default = true,
            };
        }

        return ast.Argument{
            .name = name,
            .value = value,
        };
    }

    // Check for underscore placeholder
    if (std.mem.eql(u8, arg, "_")) {
        return ast.Argument{
            .value = "",
            .use_default = true,
        };
    }

    // Positional argument
    return ast.Argument{
        .value = arg,
    };
}

fn loadJakefile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    const source = try allocator.alloc(u8, stat.size);
    errdefer allocator.free(source);

    const bytes_read = try file.readAll(source);
    if (bytes_read != stat.size) {
        return error.UnexpectedEndOfFile;
    }

    return source;
}

test "parsePreConfig extracts jakefile flag" {
    const args = [_][]const u8{ "-f", "custom.jake", "build", "arg1" };
    const config = try parsePreConfig(&args);

    try std.testing.expectEqualStrings("custom.jake", config.jakefile);
    try std.testing.expectEqual(@as(usize, 2), config.raw_args.len);
    try std.testing.expectEqualStrings("build", config.raw_args[0]);
    try std.testing.expectEqualStrings("arg1", config.raw_args[1]);
}

test "parsePreConfig handles help flag" {
    const args = [_][]const u8{ "-h", "build" };
    const config = try parsePreConfig(&args);

    try std.testing.expect(config.show_help);
}

test "parsePreConfig collects raw args" {
    const args = [_][]const u8{ "build", "arm64", "test" };
    const config = try parsePreConfig(&args);

    try std.testing.expectEqual(@as(usize, 3), config.raw_args.len);
    try std.testing.expectEqualStrings("build", config.raw_args[0]);
    try std.testing.expectEqualStrings("arm64", config.raw_args[1]);
    try std.testing.expectEqualStrings("test", config.raw_args[2]);
}

test "parseArgument handles underscore placeholder" {
    const arg = parseArgument("_");
    try std.testing.expect(arg.use_default);
    try std.testing.expect(arg.name == null);
    try std.testing.expectEqualStrings("", arg.value);
}

test "parseArgument handles named underscore placeholder" {
    const arg = parseArgument("arch=_");
    try std.testing.expect(arg.use_default);
    try std.testing.expectEqualStrings("arch", arg.name.?);
    try std.testing.expectEqualStrings("", arg.value);
}

test "parseArgument handles named arg" {
    const arg = parseArgument("arch=arm64");
    try std.testing.expect(!arg.use_default);
    try std.testing.expectEqualStrings("arch", arg.name.?);
    try std.testing.expectEqualStrings("arm64", arg.value);
}

test "parseArgument handles positional arg" {
    const arg = parseArgument("arm64");
    try std.testing.expect(!arg.use_default);
    try std.testing.expect(arg.name == null);
    try std.testing.expectEqualStrings("arm64", arg.value);
}
