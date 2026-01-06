const std = @import("std");
const parser = @import("parser.zig");
const analyzer = @import("analyzer.zig");
const executor = @import("executor.zig");
const diagnostics = @import("diagnostics.zig");
const ast = @import("ast.zig");

const Config = struct {
    jakefile: []const u8,
    target: ?[]const u8,
    args: std.array_list.Managed(ast.Argument),
    show_help: bool,

    fn deinit(self: *Config) void {
        self.args.deinit();
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

    var config = parseArgs(allocator, args[1..]) catch |err| {
        switch (err) {
            error.MissingJakefilePath => {
                try diagnostics.printExecutionError(stderr, "argument", "-f requires a file path", use_color);
            },
            error.OutOfMemory => {
                try diagnostics.printExecutionError(stderr, "memory", "out of memory", use_color);
            },
        }
        std.process.exit(1);
    };
    defer config.deinit();

    if (config.show_help) {
        try diagnostics.printHelp(stdout, use_color);
        return;
    }

    // Find and load Jakefile
    const jakefile_path = config.jakefile;
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

    // No target specified - list available targets
    if (config.target == null) {
        try diagnostics.printTargetList(stdout, &parsed.functions, use_color);
        return;
    }

    // Execute target
    var exec = executor.Executor.init(allocator, &parsed.functions, source);
    const result = exec.execute(config.target.?, config.args.items);

    switch (result) {
        .ok => {},
        .validation_err => |ve| {
            try diagnostics.printValidationError(stderr, ve, config.target.?, use_color);
            std.process.exit(1);
        },
        .err => |err| {
            switch (err) {
                error.UndefinedFunction => {
                    try diagnostics.printExecutionError(stderr, "undefined function", config.target.?, use_color);
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

fn parseArgs(allocator: std.mem.Allocator, args: []const []const u8) !Config {
    var config = Config{
        .jakefile = "Jakefile",
        .target = null,
        .args = std.array_list.Managed(ast.Argument).init(allocator),
        .show_help = false,
    };

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
            continue;
        }

        if (arg.len > 0 and arg[0] == '-') {
            // Unknown flag, skip for now
            continue;
        }

        if (config.target == null) {
            config.target = arg;
        } else {
            // Parse as function argument
            try config.args.append(parseArgument(arg));
        }
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

test "parseArgs parses target" {
    const allocator = std.testing.allocator;
    const args = [_][]const u8{"build"};

    var config = try parseArgs(allocator, &args);
    defer config.deinit();

    try std.testing.expectEqualStrings("build", config.target.?);
}

test "parseArgs parses positional args" {
    const allocator = std.testing.allocator;
    const args = [_][]const u8{ "test", "arm64", "5" };

    var config = try parseArgs(allocator, &args);
    defer config.deinit();

    try std.testing.expectEqualStrings("test", config.target.?);
    try std.testing.expectEqual(@as(usize, 2), config.args.items.len);
    try std.testing.expectEqualStrings("arm64", config.args.items[0].value);
    try std.testing.expectEqualStrings("5", config.args.items[1].value);
}

test "parseArgs parses named args" {
    const allocator = std.testing.allocator;
    const args = [_][]const u8{ "test", "arch=arm64", "num=5" };

    var config = try parseArgs(allocator, &args);
    defer config.deinit();

    try std.testing.expectEqualStrings("arm64", config.args.items[0].value);
    try std.testing.expectEqualStrings("arch", config.args.items[0].name.?);
}

test "parseArgs handles -f flag" {
    const allocator = std.testing.allocator;
    const args = [_][]const u8{ "-f", "custom.jake", "build" };

    var config = try parseArgs(allocator, &args);
    defer config.deinit();

    try std.testing.expectEqualStrings("custom.jake", config.jakefile);
    try std.testing.expectEqualStrings("build", config.target.?);
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

test "parseArgs with underscore placeholder" {
    const allocator = std.testing.allocator;
    const args = [_][]const u8{ "test", "_", "5" };

    var config = try parseArgs(allocator, &args);
    defer config.deinit();

    try std.testing.expectEqual(@as(usize, 2), config.args.items.len);
    try std.testing.expect(config.args.items[0].use_default);
    try std.testing.expectEqualStrings("5", config.args.items[1].value);
    try std.testing.expect(!config.args.items[1].use_default);
}
