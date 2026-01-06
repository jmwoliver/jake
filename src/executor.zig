const std = @import("std");
const ast = @import("ast.zig");
const analyzer = @import("analyzer.zig");

pub const ExecutorError = error{
    CommandFailed,
    UndefinedVariable,
    UndefinedFunction,
    OutOfMemory,
};

/// Result of execution - either success or detailed error
pub const ExecuteResult = union(enum) {
    ok: void,
    validation_err: analyzer.ValidationError,
    err: ExecutorError,
};

pub const Executor = struct {
    allocator: std.mem.Allocator,
    functions: *const std.StringHashMap(ast.FunctionDef),
    source: []const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        functions: *const std.StringHashMap(ast.FunctionDef),
        source: []const u8,
    ) Executor {
        return .{
            .allocator = allocator,
            .functions = functions,
            .source = source,
        };
    }

    pub fn execute(
        self: *Executor,
        function_name: []const u8,
        cli_args: []const ast.Argument,
    ) ExecuteResult {
        const func = self.functions.get(function_name) orelse {
            return .{ .err = error.UndefinedFunction };
        };

        // Resolve arguments using analyzer's validation
        var validation_result = analyzer.validateArguments(self.allocator, &func, cli_args) catch {
            return .{ .err = error.OutOfMemory };
        };

        switch (validation_result) {
            .err => |ve| return .{ .validation_err = ve },
            .ok => |*scope| {
                defer scope.deinit();

                // Execute each statement
                for (func.body) |stmt| {
                    self.executeStatement(stmt, scope) catch |err| {
                        return .{ .err = err };
                    };
                }
                return .{ .ok = {} };
            },
        }
    }

    fn executeStatement(
        self: *Executor,
        stmt: ast.Statement,
        scope: *std.StringHashMap([]const u8),
    ) ExecutorError!void {
        switch (stmt) {
            .shell_command => |cmd| {
                try self.executeShellCommand(&cmd, scope);
            },
            .jake_call => |call| {
                try self.executeJakeCall(&call, scope);
            },
            .match_statement => |match_stmt| {
                try self.executeMatchStatement(&match_stmt, scope);
            },
        }
    }

    /// Execute a match statement by finding the matching arm and executing its body
    fn executeMatchStatement(
        self: *Executor,
        match_stmt: *const ast.MatchStatement,
        scope: *std.StringHashMap([]const u8),
    ) ExecutorError!void {
        const var_value = scope.get(match_stmt.variable) orelse
            return error.UndefinedVariable;

        // Find matching arm
        for (match_stmt.arms) |arm| {
            for (arm.values) |value| {
                if (std.mem.eql(u8, var_value, value)) {
                    // Execute this arm's body
                    for (arm.body) |stmt| {
                        try self.executeStatement(stmt, scope);
                    }
                    return;
                }
            }
        }

        // Should never reach here if analyzer did its job
        unreachable;
    }

    /// Execute a shell command by building from parts and interpolating variables
    fn executeShellCommand(
        self: *Executor,
        cmd: *const ast.ShellCommand,
        scope: *std.StringHashMap([]const u8),
    ) ExecutorError!void {
        // Build command string from parts
        var result = std.array_list.Managed(u8).init(self.allocator);
        defer result.deinit();

        for (cmd.parts) |part| {
            switch (part) {
                .text => |text| {
                    result.appendSlice(text) catch return error.OutOfMemory;
                },
                .variable => |var_name| {
                    const value = scope.get(var_name) orelse return error.UndefinedVariable;
                    result.appendSlice(value) catch return error.OutOfMemory;
                },
            }
        }

        const command = result.toOwnedSlice() catch return error.OutOfMemory;
        defer self.allocator.free(command);

        // Execute via shell
        try self.runShellCommand(command);
    }

    fn executeJakeCall(
        self: *Executor,
        call: *const ast.JakeCall,
        parent_scope: *std.StringHashMap([]const u8),
    ) ExecutorError!void {
        const func = self.functions.get(call.name) orelse {
            return error.UndefinedFunction;
        };

        // Build arguments for the called function
        var args = std.array_list.Managed(ast.Argument).init(self.allocator);
        defer args.deinit();

        for (call.args) |arg| {
            const value = switch (arg) {
                .string_literal => |s| s,
                .variable_ref => |v| parent_scope.get(v) orelse return error.UndefinedVariable,
            };
            args.append(.{ .value = value }) catch return error.OutOfMemory;
        }

        // Recursively execute
        var validation_result = analyzer.validateArguments(self.allocator, &func, args.items) catch {
            return error.OutOfMemory;
        };

        switch (validation_result) {
            .err => {
                // Internal call validation error - this shouldn't happen if analyzer did its job
                // For now, treat as a command failure
                std.debug.print("Internal error: invalid arguments to function '{s}'\n", .{call.name});
                return error.CommandFailed;
            },
            .ok => |*scope| {
                defer scope.deinit();
                for (func.body) |stmt| {
                    try self.executeStatement(stmt, scope);
                }
            },
        }
    }

    fn runShellCommand(self: *Executor, cmd: []const u8) ExecutorError!void {
        _ = self;

        // Print the command being run
        std.debug.print("$ {s}\n", .{cmd});

        var child = std.process.Child.init(
            &[_][]const u8{ "/bin/sh", "-c", cmd },
            std.heap.page_allocator,
        );
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;

        child.spawn() catch return error.CommandFailed;
        const term = child.wait() catch return error.CommandFailed;

        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    std.debug.print("Command failed with exit code {d}\n", .{code});
                    return error.CommandFailed;
                }
            },
            .Signal => |sig| {
                std.debug.print("Command killed by signal {d}\n", .{sig});
                return error.CommandFailed;
            },
            else => return error.CommandFailed,
        }
    }
};
