const std = @import("std");
const ast = @import("ast.zig");
const analyzer = @import("analyzer.zig");
const executor = @import("executor.zig");

pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    loc: ast.Location,
    hint: ?[]const u8,
    source: []const u8,
    filename: []const u8,

    pub const Severity = enum {
        err,
        warning,
        note,

        pub fn toString(self: Severity) []const u8 {
            return switch (self) {
                .err => "error",
                .warning => "warning",
                .note => "note",
            };
        }

        pub fn color(self: Severity) []const u8 {
            return switch (self) {
                .err => "\x1b[1;31m", // bold red
                .warning => "\x1b[1;33m", // bold yellow
                .note => "\x1b[1;36m", // bold cyan
            };
        }
    };
};

pub fn printParseError(
    writer: anytype,
    err: ast.ParseError,
    source: []const u8,
    filename: []const u8,
    use_color: bool,
) !void {
    try printDiagnostic(writer, .{
        .severity = .err,
        .message = err.message,
        .loc = err.loc,
        .hint = null,
        .source = source,
        .filename = filename,
    }, use_color);
}

pub fn printAnalyzerError(
    writer: anytype,
    err: analyzer.AnalyzerError,
    source: []const u8,
    filename: []const u8,
    use_color: bool,
) !void {
    try printDiagnostic(writer, .{
        .severity = .err,
        .message = err.message,
        .loc = err.loc,
        .hint = err.hint,
        .source = source,
        .filename = filename,
    }, use_color);
}

pub fn printDiagnostic(
    writer: anytype,
    diag: Diagnostic,
    use_color: bool,
) !void {
    const reset = if (use_color) "\x1b[0m" else "";
    const bold = if (use_color) "\x1b[1m" else "";
    const dim = if (use_color) "\x1b[2m" else "";
    const cyan = if (use_color) "\x1b[36m" else "";
    const severity_color = if (use_color) diag.severity.color() else "";

    // error: message
    try writer.print("{s}{s}{s}: {s}{s}\n", .{
        severity_color,
        diag.severity.toString(),
        reset,
        bold,
        diag.message,
    });
    try writer.print("{s}", .{reset});

    // --> filename:line:column
    try writer.print("{s}  --> {s}{s}:{d}:{d}\n", .{
        cyan,
        reset,
        diag.filename,
        diag.loc.line,
        diag.loc.column,
    });

    // Source line with context
    const source_line = getSourceLine(diag.source, diag.loc.line);
    if (source_line) |line| {
        // Line number gutter
        try writer.print("{s}   |{s}\n", .{ dim, reset });
        try writer.print("{s}{d: >3} |{s} {s}\n", .{
            dim,
            diag.loc.line,
            reset,
            line,
        });

        // Underline
        try writer.print("{s}   |{s} ", .{ dim, reset });
        var i: u32 = 1;
        while (i < diag.loc.column) : (i += 1) {
            try writer.writeByte(' ');
        }
        try writer.print("{s}", .{severity_color});

        // Calculate span length
        const span_len = if (diag.loc.end > diag.loc.start)
            @min(diag.loc.end - diag.loc.start, line.len - diag.loc.column + 1)
        else
            1;

        var j: u32 = 0;
        while (j < span_len) : (j += 1) {
            try writer.writeByte('^');
        }
        try writer.print("{s}\n", .{reset});
    }

    // Hint
    if (diag.hint) |hint| {
        try writer.print("{s}   = {s}hint: {s}\n", .{ dim, reset, hint });
    }

    try writer.writeByte('\n');
}

fn getSourceLine(source: []const u8, target_line: u32) ?[]const u8 {
    if (target_line == 0) return null;

    var current_line: u32 = 1;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (current_line == target_line) {
                return source[line_start..i];
            }
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Last line (no trailing newline)
    if (current_line == target_line and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}

pub fn printExecutionError(
    writer: anytype,
    err_type: []const u8,
    message: []const u8,
    use_color: bool,
) !void {
    const reset = if (use_color) "\x1b[0m" else "";
    const bold_red = if (use_color) "\x1b[1;31m" else "";

    try writer.print("{s}error{s}: {s}: {s}\n", .{
        bold_red,
        reset,
        err_type,
        message,
    });
}

pub fn printValidationError(
    writer: anytype,
    ve: analyzer.ValidationError,
    target_name: []const u8,
    use_color: bool,
) !void {
    const reset = if (use_color) "\x1b[0m" else "";
    const bold_red = if (use_color) "\x1b[1;31m" else "";
    const bold = if (use_color) "\x1b[1m" else "";
    const cyan = if (use_color) "\x1b[36m" else "";

    switch (ve.kind) {
        .missing_required_param => {
            try writer.print("{s}error{s}: missing required parameter '{s}{s}{s}' for target '{s}'\n", .{
                bold_red,
                reset,
                bold,
                ve.param_name,
                reset,
                target_name,
            });
            if (ve.options) |opts| {
                try writer.print("       allowed values: ", .{});
                for (opts, 0..) |opt, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("{s}{s}{s}", .{ cyan, opt, reset });
                }
                try writer.print("\n", .{});
            }
        },
        .invalid_param_value => {
            try writer.print("{s}error{s}: invalid value '{s}' for parameter '{s}{s}{s}'\n", .{
                bold_red,
                reset,
                ve.value orelse "?",
                bold,
                ve.param_name,
                reset,
            });
            if (ve.options) |opts| {
                try writer.print("       allowed values: ", .{});
                for (opts, 0..) |opt, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("{s}{s}{s}", .{ cyan, opt, reset });
                }
                try writer.print("\n", .{});
            }
        },
        .too_many_arguments => {
            try writer.print("{s}error{s}: too many arguments for target '{s}'\n", .{
                bold_red,
                reset,
                target_name,
            });
            if (ve.value) |v| {
                try writer.print("       unexpected argument: {s}\n", .{v});
            }
        },
        .unknown_parameter => {
            try writer.print("{s}error{s}: unknown parameter '{s}{s}{s}' for target '{s}'\n", .{
                bold_red,
                reset,
                bold,
                ve.param_name,
                reset,
                target_name,
            });
        },
        .placeholder_no_default => {
            try writer.print("{s}error{s}: cannot use '_' placeholder for parameter '{s}{s}{s}' (no default value)\n", .{
                bold_red,
                reset,
                bold,
                ve.param_name,
                reset,
            });
            try writer.print("       hint: parameter '{s}' requires an explicit value\n", .{ve.param_name});
        },
    }
}

pub fn printChainValidationError(
    writer: anytype,
    cve: executor.ChainValidationError,
    use_color: bool,
) !void {
    try printValidationError(writer, cve.err, cve.target_name, use_color);
}

pub fn printTargetList(
    writer: anytype,
    functions: *const std.StringHashMap(ast.FunctionDef),
    use_color: bool,
) !void {
    const reset = if (use_color) "\x1b[0m" else "";
    const bold = if (use_color) "\x1b[1m" else "";
    const green = if (use_color) "\x1b[32m" else "";
    const dim = if (use_color) "\x1b[2m" else "";

    try writer.print("{s}Available targets:{s}\n\n", .{ bold, reset });

    // Collect and sort function names for consistent output
    var names = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
    defer names.deinit();

    var iter = functions.keyIterator();
    while (iter.next()) |key| {
        try names.append(key.*);
    }

    std.mem.sort([]const u8, names.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    for (names.items) |name| {
        const func = functions.get(name).?;
        try writer.print("  {s}{s}{s}", .{ green, name, reset });

        if (func.params.len > 0) {
            try writer.print("(", .{});
            for (func.params, 0..) |param, i| {
                if (i > 0) try writer.print(", ", .{});

                try writer.print("{s}", .{param.name});

                if (param.default) |def| {
                    try writer.print("{s}={s}{s}", .{ dim, def, reset });
                }

                if (param.options) |opts| {
                    try writer.print(" {s}[", .{dim});
                    for (opts, 0..) |opt, j| {
                        if (j > 0) try writer.print("|", .{});
                        try writer.print("{s}", .{opt});
                    }
                    try writer.print("]{s}", .{reset});
                }
            }
            try writer.print(")", .{});
        }

        try writer.writeByte('\n');
    }
}

pub fn printHelp(writer: anytype, use_color: bool) !void {
    const reset = if (use_color) "\x1b[0m" else "";
    const bold = if (use_color) "\x1b[1m" else "";
    const green = if (use_color) "\x1b[32m" else "";

    try writer.print("{s}jake{s} - A fast command runner\n\n", .{ bold, reset });
    try writer.print("{s}USAGE:{s}\n", .{ bold, reset });
    try writer.print("    jake [OPTIONS] [TARGET [ARGS...]]...\n\n", .{});
    try writer.print("{s}OPTIONS:{s}\n", .{ bold, reset });
    try writer.print("    {s}-f <FILE>{s}    Use specified Jakefile\n", .{ green, reset });
    try writer.print("    {s}-h, --help{s}  Show this help message\n\n", .{ green, reset });
    try writer.print("{s}ARGS:{s}\n", .{ bold, reset });
    try writer.print("    Positional:  jake build arm64\n", .{});
    try writer.print("    Named:       jake build arch=arm64\n", .{});
    try writer.print("    Placeholder: jake test _ 10       (use '_' for default value)\n\n", .{});
    try writer.print("{s}CHAINING:{s}\n", .{ bold, reset });
    try writer.print("    Multiple targets can be chained in a single command.\n", .{});
    try writer.print("    Arguments are consumed based on each target's parameter count.\n\n", .{});
    try writer.print("{s}EXAMPLES:{s}\n", .{ bold, reset });
    try writer.print("    jake                     List available targets\n", .{});
    try writer.print("    jake build               Run 'build' with defaults\n", .{});
    try writer.print("    jake build arm64 debug   Run 'build' with args\n", .{});
    try writer.print("    jake build _ release     Use default for first arg\n", .{});
    try writer.print("    jake one two             Chain targets 'one' then 'two'\n", .{});
    try writer.print("    jake build arm64 test    Run 'build' with arg, then 'test'\n", .{});
}

// Tests
test "getSourceLine returns correct line" {
    const source = "line1\nline2\nline3";
    try std.testing.expectEqualStrings("line1", getSourceLine(source, 1).?);
    try std.testing.expectEqualStrings("line2", getSourceLine(source, 2).?);
    try std.testing.expectEqualStrings("line3", getSourceLine(source, 3).?);
    try std.testing.expect(getSourceLine(source, 4) == null);
}

test "getSourceLine handles no trailing newline" {
    const source = "only line";
    try std.testing.expectEqualStrings("only line", getSourceLine(source, 1).?);
}
