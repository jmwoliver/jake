const std = @import("std");

pub const ParamSpec = struct {
    name: []const u8,
    default: ?[]const u8,
    options: ?[]const []const u8, // null = any value allowed

    pub fn isRequired(self: ParamSpec) bool {
        return self.default == null;
    }

    pub fn isValidValue(self: ParamSpec, value: []const u8) bool {
        if (self.options) |opts| {
            for (opts) |opt| {
                if (std.mem.eql(u8, opt, value)) return true;
            }
            return false;
        }
        return true; // No constraints
    }
};

pub const Argument = struct {
    value: []const u8,
    name: ?[]const u8 = null, // For named args like "arch=arm64"
    use_default: bool = false, // True when "_" placeholder is used

    pub fn isNamed(self: Argument) bool {
        return self.name != null;
    }

    pub fn isPlaceholder(self: Argument) bool {
        return self.use_default;
    }
};

// Statement types for shell-first syntax
pub const Statement = union(enum) {
    shell_command: ShellCommand,
    jake_call: JakeCall,
    match_statement: MatchStatement,
};

/// A single arm of a match statement
pub const MatchArm = struct {
    values: []const []const u8, // One or more values (for multi-value arms like "release | fast")
    body: []const Statement,
};

/// A match statement for exhaustive branching on constrained parameters
pub const MatchStatement = struct {
    variable: []const u8, // Variable name (without $)
    variable_loc: Location, // Location for error reporting
    arms: []const MatchArm,
};

/// A shell command line with variable interpolation
pub const ShellCommand = struct {
    parts: []const CommandPart,
};

/// Part of a shell command - either raw text or a variable reference
pub const CommandPart = union(enum) {
    text: []const u8,
    variable: []const u8, // Variable name without the $
};

/// A call to another jake function (prefixed with @ in syntax)
pub const JakeCall = struct {
    name: []const u8,
    name_loc: Location,
    args: []const CallArg,
};

pub const CallArg = union(enum) {
    string_literal: []const u8,
    variable_ref: []const u8,
};

pub const FunctionDef = struct {
    name: []const u8,
    name_loc: Location,
    params: []const ParamSpec,
    body: []const Statement,
};

pub const Location = struct {
    line: u32,
    column: u32,
    start: u32,
    end: u32,
};

pub const Ast = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    functions: std.StringHashMap(FunctionDef),
    errors: std.array_list.Managed(ParseError),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Ast {
        return .{
            .allocator = allocator,
            .source = source,
            .functions = std.StringHashMap(FunctionDef).init(allocator),
            .errors = std.array_list.Managed(ParseError).init(allocator),
        };
    }

    pub fn deinit(self: *Ast) void {
        // Free function bodies and params
        var iter = self.functions.valueIterator();
        while (iter.next()) |func| {
            self.freeStatements(func.body);
            self.allocator.free(func.body);
            for (func.params) |param| {
                if (param.options) |opts| {
                    self.allocator.free(opts);
                }
            }
            self.allocator.free(func.params);
        }
        self.functions.deinit();
        self.errors.deinit();
    }

    fn freeStatements(self: *Ast, statements: []const Statement) void {
        for (statements) |stmt| {
            switch (stmt) {
                .shell_command => |cmd| {
                    self.allocator.free(cmd.parts);
                },
                .jake_call => |call| {
                    self.allocator.free(call.args);
                },
                .match_statement => |match_stmt| {
                    for (match_stmt.arms) |arm| {
                        self.freeStatements(arm.body);
                        self.allocator.free(arm.body);
                        self.allocator.free(arm.values);
                    }
                    self.allocator.free(match_stmt.arms);
                },
            }
        }
    }

    pub fn hasErrors(self: *const Ast) bool {
        return self.errors.items.len > 0;
    }
};

pub const ParseError = struct {
    kind: Kind,
    loc: Location,
    message: []const u8,

    pub const Kind = enum {
        unexpected_token,
        expected_token,
        unterminated_string,
        duplicate_function,
        invalid_param_spec,
    };
};
