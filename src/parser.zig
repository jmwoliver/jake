const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const ast = @import("ast.zig");

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    current: Token,
    previous: Token,
    source: []const u8,
    parsed_ast: ast.Ast,

    /// Free a single statement and its nested allocations
    fn freeStatement(self: *Parser, stmt: ast.Statement) void {
        switch (stmt) {
            .shell_command => |cmd| self.allocator.free(cmd.parts),
            .jake_call => |call| self.allocator.free(call.args),
            .match_statement => |match_stmt| {
                for (match_stmt.arms) |arm| {
                    for (arm.body) |s| {
                        self.freeStatement(s);
                    }
                    self.allocator.free(arm.body);
                    self.allocator.free(arm.values);
                }
                self.allocator.free(match_stmt.arms);
            },
            .var_assignment => |va| self.allocator.free(va.value),
        }
    }

    /// Free a body slice and all nested allocations
    fn freeBody(self: *Parser, body: []const ast.Statement) void {
        for (body) |stmt| {
            self.freeStatement(stmt);
        }
        self.allocator.free(body);
    }

    /// Free a params slice and all nested allocations (options)
    fn freeParams(self: *Parser, params: []const ast.ParamSpec) void {
        for (params) |param| {
            if (param.options) |opts| {
                self.allocator.free(opts);
            }
        }
        self.allocator.free(params);
    }

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        var lexer = Lexer.init(source);
        const first_token = lexer.next();
        return .{
            .allocator = allocator,
            .lexer = lexer,
            .current = first_token,
            .previous = first_token,
            .source = source,
            .parsed_ast = ast.Ast.init(allocator, source),
        };
    }

    pub fn parse(allocator: std.mem.Allocator, source: []const u8) ast.Ast {
        var parser = Parser.init(allocator, source);
        parser.parseFile();
        return parser.parsed_ast;
    }

    fn parseFile(self: *Parser) void {
        while (self.current.tag != .eof) {
            self.skipNewlines();
            if (self.current.tag == .eof) break;

            if (self.current.tag == .identifier) {
                self.parseFunctionDef();
            } else {
                self.addError(.unexpected_token, self.currentLoc(), "expected function definition");
                self.advance();
            }
        }
    }

    fn parseFunctionDef(self: *Parser) void {
        const name_token = self.current;
        const name = self.tokenSlice(name_token);
        const name_loc = self.currentLoc();

        self.advance(); // consume identifier

        if (!self.expect(.l_paren, "expected '(' after function name")) return;

        const params = self.parseParamList() catch {
            self.synchronize();
            return;
        };

        if (!self.expect(.r_paren, "expected ')' after parameters")) {
            self.freeParams(params);
            return;
        }

        self.skipNewlines();

        if (!self.expect(.l_brace, "expected '{' to start function body")) {
            self.freeParams(params);
            return;
        }

        const body = self.parseBody() catch {
            self.freeParams(params);
            self.synchronize();
            return;
        };

        if (!self.expect(.r_brace, "expected '}' to end function body")) {
            self.freeParams(params);
            self.freeBody(body);
            return;
        }

        // Check for duplicate
        if (self.parsed_ast.functions.contains(name)) {
            self.addError(.duplicate_function, name_loc, "duplicate function definition");
            self.freeParams(params);
            self.freeBody(body);
            return;
        }

        self.parsed_ast.functions.put(name, ast.FunctionDef{
            .name = name,
            .name_loc = name_loc,
            .params = params,
            .body = body,
        }) catch {};
    }

    fn parseParamList(self: *Parser) ![]const ast.ParamSpec {
        var params = std.array_list.Managed(ast.ParamSpec).init(self.allocator);
        errdefer params.deinit();

        if (self.current.tag == .r_paren) {
            return params.toOwnedSlice();
        }

        try params.append(try self.parseParam());

        while (self.current.tag == .comma) {
            self.advance(); // consume comma
            try params.append(try self.parseParam());
        }

        return params.toOwnedSlice();
    }

    /// Parse parameter with new syntax: name or name: {opt1|*opt2|opt3}
    fn parseParam(self: *Parser) !ast.ParamSpec {
        if (self.current.tag != .identifier) {
            self.addError(.expected_token, self.currentLoc(), "expected parameter name");
            return error.ParseError;
        }

        const name = self.tokenSlice(self.current);
        self.advance();

        var default: ?[]const u8 = null;
        var options: ?[]const []const u8 = null;

        // Check for colon - indicates constrained options: name: {opt1|*opt2}
        if (self.current.tag == .colon) {
            self.advance(); // consume :

            if (self.current.tag != .l_brace) {
                self.addError(.expected_token, self.currentLoc(), "expected '{' after ':'");
                return error.ParseError;
            }
            self.advance(); // consume {

            // Parse options: {opt1|*opt2|opt3}
            var opts = std.array_list.Managed([]const u8).init(self.allocator);
            errdefer opts.deinit();

            while (self.current.tag != .r_brace and self.current.tag != .eof) {
                // Check for * (default marker)
                const is_default = self.current.tag == .asterisk;
                if (is_default) {
                    if (default != null) {
                        self.addError(.invalid_param_spec, self.currentLoc(), "multiple defaults specified");
                        return error.ParseError;
                    }
                    self.advance(); // consume *
                }

                if (self.current.tag != .identifier) {
                    self.addError(.expected_token, self.currentLoc(), "expected option value");
                    return error.ParseError;
                }

                const opt = self.tokenSlice(self.current);
                try opts.append(opt);
                if (is_default) {
                    default = opt;
                }
                self.advance();

                if (self.current.tag == .pipe) {
                    self.advance(); // consume |
                } else {
                    break;
                }
            }

            if (self.current.tag != .r_brace) {
                self.addError(.expected_token, self.currentLoc(), "expected '}' to close options");
                return error.ParseError;
            }
            self.advance(); // consume }

            options = try opts.toOwnedSlice();
        }
        // If no colon, param is unconstrained and required (no default)

        return ast.ParamSpec{
            .name = name,
            .default = default,
            .options = options,
        };
    }

    const ParseError = error{ParseError, OutOfMemory};

    /// Parse function body - shell commands by default, @func() for jake calls
    fn parseBody(self: *Parser) ParseError![]const ast.Statement {
        var statements = std.array_list.Managed(ast.Statement).init(self.allocator);
        errdefer {
            for (statements.items) |stmt| {
                self.freeStatement(stmt);
            }
            statements.deinit();
        }

        self.skipNewlines();

        // Track base indentation for continuation detection
        var pending_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
        defer pending_parts.deinit();

        while (self.current.tag != .r_brace and self.current.tag != .eof) {
            if (self.current.tag == .kw_match) {
                // Flush any pending shell command
                if (pending_parts.items.len > 0) {
                    try statements.append(.{ .shell_command = .{
                        .parts = try pending_parts.toOwnedSlice(),
                    } });
                    pending_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
                }

                // Match statement
                const match_stmt = try self.parseMatchStatement();
                try statements.append(.{ .match_statement = match_stmt });
            } else if (self.current.tag == .at_sign) {
                // Flush any pending shell command
                if (pending_parts.items.len > 0) {
                    try statements.append(.{ .shell_command = .{
                        .parts = try pending_parts.toOwnedSlice(),
                    } });
                    pending_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
                }

                // Jake function call: @func(args)
                self.advance(); // consume @
                const call = try self.parseJakeCall();
                try statements.append(.{ .jake_call = call });
            } else if (self.current.tag == .newline) {
                // End of line - flush pending command if any
                if (pending_parts.items.len > 0) {
                    try statements.append(.{ .shell_command = .{
                        .parts = try pending_parts.toOwnedSlice(),
                    } });
                    pending_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
                }
                self.advance();
            } else if (self.current.tag == .identifier and self.isVarAssignment()) {
                // Flush any pending shell command
                if (pending_parts.items.len > 0) {
                    try statements.append(.{ .shell_command = .{
                        .parts = try pending_parts.toOwnedSlice(),
                    } });
                    pending_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
                }

                // Variable assignment: name = value
                const var_assign = try self.parseVarAssignment();
                try statements.append(.{ .var_assignment = var_assign });
            } else {
                // Shell command content
                try self.parseShellContent(&pending_parts);
            }
        }

        // Flush final pending command
        if (pending_parts.items.len > 0) {
            try statements.append(.{ .shell_command = .{
                .parts = try pending_parts.toOwnedSlice(),
            } });
        }

        return statements.toOwnedSlice();
    }

    /// Check if current position looks like a variable assignment (identifier = ...)
    fn isVarAssignment(self: *Parser) bool {
        // Save current position to peek ahead
        var temp_lexer = self.lexer;
        const next_tok = temp_lexer.next();
        return next_tok.tag == .eq;
    }

    /// Parse variable assignment: name = value
    fn parseVarAssignment(self: *Parser) ParseError!ast.VarAssignment {
        const name = self.tokenSlice(self.current);
        self.advance(); // consume identifier

        if (self.current.tag != .eq) {
            self.addError(.expected_token, self.currentLoc(), "expected '=' in variable assignment");
            return error.ParseError;
        }
        self.advance(); // consume =

        // Parse the value (everything until newline or r_brace)
        var value_parts = std.array_list.Managed(ast.CommandPart).init(self.allocator);
        errdefer value_parts.deinit();

        try self.parseShellContent(&value_parts);

        return .{
            .name = name,
            .value = try value_parts.toOwnedSlice(),
        };
    }

    /// Parse match statement: match $variable { value: { ... } ... }
    fn parseMatchStatement(self: *Parser) ParseError!ast.MatchStatement {
        self.advance(); // consume 'match'

        // Expect $variable
        if (self.current.tag != .dollar) {
            self.addError(.expected_token, self.currentLoc(), "expected '$' before variable in match");
            return error.ParseError;
        }
        self.advance(); // consume $

        if (self.current.tag != .identifier) {
            self.addError(.expected_token, self.currentLoc(), "expected variable name after '$'");
            return error.ParseError;
        }
        const variable = self.tokenSlice(self.current);
        const variable_loc = self.currentLoc();
        self.advance();

        self.skipNewlines();

        // Expect {
        if (self.current.tag != .l_brace) {
            self.addError(.expected_token, self.currentLoc(), "expected '{' after match variable");
            return error.ParseError;
        }
        self.advance(); // consume {

        // Parse arms
        var arms = std.array_list.Managed(ast.MatchArm).init(self.allocator);
        errdefer {
            for (arms.items) |arm| {
                for (arm.body) |stmt| {
                    self.freeStatement(stmt);
                }
                self.allocator.free(arm.body);
                self.allocator.free(arm.values);
            }
            arms.deinit();
        }

        self.skipNewlines();

        while (self.current.tag != .r_brace and self.current.tag != .eof) {
            const arm = try self.parseMatchArm();
            try arms.append(arm);
            self.skipNewlines();
        }

        if (self.current.tag != .r_brace) {
            self.addError(.expected_token, self.currentLoc(), "expected '}' to close match");
            return error.ParseError;
        }
        self.advance(); // consume }

        return .{
            .variable = variable,
            .variable_loc = variable_loc,
            .arms = try arms.toOwnedSlice(),
        };
    }

    /// Parse a single match arm: value | value2: { ... }
    fn parseMatchArm(self: *Parser) ParseError!ast.MatchArm {
        var values = std.array_list.Managed([]const u8).init(self.allocator);
        errdefer values.deinit();

        // Parse first value
        if (self.current.tag != .identifier) {
            self.addError(.expected_token, self.currentLoc(), "expected value in match arm");
            return error.ParseError;
        }
        try values.append(self.tokenSlice(self.current));
        self.advance();

        // Parse additional values separated by |
        while (self.current.tag == .pipe) {
            self.advance(); // consume |
            if (self.current.tag != .identifier) {
                self.addError(.expected_token, self.currentLoc(), "expected value after '|'");
                return error.ParseError;
            }
            try values.append(self.tokenSlice(self.current));
            self.advance();
        }

        // Expect :
        if (self.current.tag != .colon) {
            self.addError(.expected_token, self.currentLoc(), "expected ':' after match value(s)");
            return error.ParseError;
        }
        self.advance(); // consume :

        self.skipNewlines();

        // Expect { for body
        if (self.current.tag != .l_brace) {
            self.addError(.expected_token, self.currentLoc(), "expected '{' after ':'");
            return error.ParseError;
        }
        self.advance(); // consume {

        const body = try self.parseBody();

        if (self.current.tag != .r_brace) {
            self.addError(.expected_token, self.currentLoc(), "expected '}' to close match arm");
            self.freeBody(body);
            return error.ParseError;
        }
        self.advance(); // consume }

        return .{
            .values = try values.toOwnedSlice(),
            .body = body,
        };
    }

    /// Parse shell command content, extracting $variable references
    fn parseShellContent(self: *Parser, parts: *std.array_list.Managed(ast.CommandPart)) !void {
        // Accumulate text until we hit a variable, newline, or end
        const line_start = self.current.loc.start;
        var text_start = line_start;

        while (self.current.tag != .newline and
            self.current.tag != .r_brace and
            self.current.tag != .eof)
        {
            if (self.current.tag == .dollar) {
                // Flush accumulated text before the $
                if (self.current.loc.start > text_start) {
                    try parts.append(.{ .text = self.source[text_start..self.current.loc.start] });
                }

                self.advance(); // consume $

                if (self.current.tag == .identifier) {
                    try parts.append(.{ .variable = self.tokenSlice(self.current) });
                    self.advance();
                    text_start = self.current.loc.start;
                } else {
                    // Lone $ - include it as text
                    try parts.append(.{ .text = "$" });
                    text_start = self.current.loc.start;
                }
            } else if (self.current.tag == .string_literal or self.current.tag == .multiline_string) {
                // Flush accumulated text before the string
                if (self.current.loc.start > text_start) {
                    try parts.append(.{ .text = self.source[text_start..self.current.loc.start] });
                }

                // Parse the string content, extracting $variables
                const raw = self.tokenSlice(self.current);
                const quote_len: usize = if (self.current.tag == .multiline_string) 3 else 1;
                const content = raw[quote_len .. raw.len - quote_len];

                try self.parseStringWithVariables(parts, content);

                self.advance();
                text_start = self.current.loc.start;
            } else {
                self.advance();
            }
        }

        // Flush remaining text
        if (self.current.loc.start > text_start) {
            try parts.append(.{ .text = self.source[text_start..self.current.loc.start] });
        }
    }

    /// Parse string content and extract $variable references
    fn parseStringWithVariables(self: *Parser, parts: *std.array_list.Managed(ast.CommandPart), content: []const u8) !void {
        _ = self;
        var i: usize = 0;
        var text_start: usize = 0;

        while (i < content.len) {
            if (content[i] == '$') {
                // Flush text before $
                if (i > text_start) {
                    try parts.append(.{ .text = content[text_start..i] });
                }

                i += 1; // skip $

                // Extract variable name
                const var_start = i;
                while (i < content.len and isIdentChar(content[i])) {
                    i += 1;
                }

                if (i > var_start) {
                    try parts.append(.{ .variable = content[var_start..i] });
                } else {
                    // Lone $ - include as text
                    try parts.append(.{ .text = "$" });
                }
                text_start = i;
            } else if (content[i] == '\\' and i + 1 < content.len) {
                // Skip escape sequences
                i += 2;
            } else {
                i += 1;
            }
        }

        // Flush remaining text
        if (i > text_start) {
            try parts.append(.{ .text = content[text_start..] });
        }
    }

    /// Parse jake function call: func(args)
    fn parseJakeCall(self: *Parser) !ast.JakeCall {
        if (self.current.tag != .identifier) {
            self.addError(.expected_token, self.currentLoc(), "expected function name after '@'");
            return error.ParseError;
        }

        const name = self.tokenSlice(self.current);
        const name_loc = self.currentLoc();
        self.advance();

        if (self.current.tag != .l_paren) {
            self.addError(.expected_token, self.currentLoc(), "expected '(' after function name");
            return error.ParseError;
        }
        self.advance(); // consume (

        const args = try self.parseCallArgs();

        if (self.current.tag != .r_paren) {
            self.addError(.expected_token, self.currentLoc(), "expected ')' after arguments");
            self.allocator.free(args);
            return error.ParseError;
        }
        self.advance(); // consume )

        return ast.JakeCall{
            .name = name,
            .name_loc = name_loc,
            .args = args,
        };
    }

    fn parseCallArgs(self: *Parser) ![]const ast.CallArg {
        var args = std.array_list.Managed(ast.CallArg).init(self.allocator);
        errdefer args.deinit();

        if (self.current.tag == .r_paren) {
            return args.toOwnedSlice();
        }

        try args.append(try self.parseCallArg());

        while (self.current.tag == .comma) {
            self.advance(); // consume comma
            try args.append(try self.parseCallArg());
        }

        return args.toOwnedSlice();
    }

    fn parseCallArg(self: *Parser) !ast.CallArg {
        if (self.current.tag == .string_literal or self.current.tag == .multiline_string) {
            const raw = self.tokenSlice(self.current);
            // Strip quotes
            const content = if (self.current.tag == .multiline_string)
                raw[3 .. raw.len - 3] // Strip """
            else
                raw[1 .. raw.len - 1]; // Strip "
            self.advance();
            return .{ .string_literal = content };
        } else if (self.current.tag == .dollar) {
            self.advance(); // consume $
            if (self.current.tag != .identifier) {
                self.addError(.expected_token, self.currentLoc(), "expected variable name after '$'");
                return error.ParseError;
            }
            const var_name = self.tokenSlice(self.current);
            self.advance();
            return .{ .variable_ref = var_name };
        } else if (self.current.tag == .identifier) {
            // Bare identifier as argument
            const value = self.tokenSlice(self.current);
            self.advance();
            return .{ .variable_ref = value };
        }

        self.addError(.expected_token, self.currentLoc(), "expected string or variable");
        return error.ParseError;
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.lexer.next();
    }

    fn expect(self: *Parser, tag: Token.Tag, message: []const u8) bool {
        if (self.current.tag == tag) {
            self.advance();
            return true;
        }
        self.addError(.expected_token, self.currentLoc(), message);
        return false;
    }

    fn skipNewlines(self: *Parser) void {
        while (self.current.tag == .newline) {
            self.advance();
        }
    }

    fn synchronize(self: *Parser) void {
        // Skip to next function definition or EOF
        while (self.current.tag != .eof) {
            if (self.current.tag == .r_brace) {
                self.advance();
                return;
            }
            self.advance();
        }
    }

    fn tokenSlice(self: *Parser, token: Token) []const u8 {
        return token.slice(self.source);
    }

    fn currentLoc(self: *Parser) ast.Location {
        return .{
            .line = self.current.loc.line,
            .column = self.current.loc.column,
            .start = self.current.loc.start,
            .end = self.current.loc.end,
        };
    }

    fn addError(self: *Parser, kind: ast.ParseError.Kind, loc: ast.Location, message: []const u8) void {
        self.parsed_ast.errors.append(.{
            .kind = kind,
            .loc = loc,
            .message = message,
        }) catch {};
    }
};

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or
        c == '_' or c == '-';
}

// Tests - Updated for new syntax
test "parser parses simple function" {
    const source = "build() { echo hello }";
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), result.functions.count());
    try std.testing.expect(result.functions.contains("build"));
}

test "parser parses function with params" {
    const source = "build(arch, mode) { echo $arch $mode }";
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    try std.testing.expectEqual(@as(usize, 2), func.params.len);
    try std.testing.expectEqualStrings("arch", func.params[0].name);
    try std.testing.expectEqualStrings("mode", func.params[1].name);
}

test "parser parses param with default using asterisk" {
    const source = "build(arch: {*arm64|x86_64}) { echo $arch }";
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    try std.testing.expectEqual(@as(usize, 1), func.params.len);
    try std.testing.expectEqualStrings("arm64", func.params[0].default.?);
    const opts = func.params[0].options.?;
    try std.testing.expectEqual(@as(usize, 2), opts.len);
    try std.testing.expectEqualStrings("arm64", opts[0]);
    try std.testing.expectEqualStrings("x86_64", opts[1]);
}

test "parser parses param with options but no default" {
    const source = "test(env: {dev|staging|prod}) { echo $env }";
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("test").?;
    try std.testing.expect(func.params[0].default == null);
    const opts = func.params[0].options.?;
    try std.testing.expectEqual(@as(usize, 3), opts.len);
}

test "parser parses jake function call with @ prefix" {
    const source =
        \\build(arch) { echo building }
        \\test(arch) {
        \\  @build($arch)
        \\  echo testing
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("test").?;
    try std.testing.expectEqual(@as(usize, 2), func.body.len);

    // First statement is jake call
    switch (func.body[0]) {
        .jake_call => |call| {
            try std.testing.expectEqualStrings("build", call.name);
        },
        else => try std.testing.expect(false),
    }

    // Second statement is shell command
    switch (func.body[1]) {
        .shell_command => {},
        else => try std.testing.expect(false),
    }
}

test "parser parses shell command with variable interpolation" {
    const source = "build(name) { echo Hello $name! }";
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    try std.testing.expectEqual(@as(usize, 1), func.body.len);

    switch (func.body[0]) {
        .shell_command => |cmd| {
            // Should have: "echo Hello ", variable "name", "!"
            try std.testing.expectEqual(@as(usize, 3), cmd.parts.len);
            switch (cmd.parts[1]) {
                .variable => |v| try std.testing.expectEqualStrings("name", v),
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses variable interpolation inside quoted strings" {
    const source =
        \\build(num) { echo "$num tests passed" }
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    try std.testing.expectEqual(@as(usize, 1), func.body.len);

    switch (func.body[0]) {
        .shell_command => |cmd| {
            // Should have: "echo ", variable "num", " tests passed"
            try std.testing.expectEqual(@as(usize, 3), cmd.parts.len);

            // First part: "echo "
            switch (cmd.parts[0]) {
                .text => |t| try std.testing.expectEqualStrings("echo ", t),
                else => try std.testing.expect(false),
            }

            // Second part: variable "num"
            switch (cmd.parts[1]) {
                .variable => |v| try std.testing.expectEqualStrings("num", v),
                else => try std.testing.expect(false),
            }

            // Third part: " tests passed"
            switch (cmd.parts[2]) {
                .text => |t| try std.testing.expectEqualStrings(" tests passed", t),
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses multiple variables in quoted string" {
    const source =
        \\run(a, b) { echo "$a and $b" }
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("run").?;

    switch (func.body[0]) {
        .shell_command => |cmd| {
            // Should have: "echo ", variable "a", " and ", variable "b"
            try std.testing.expectEqual(@as(usize, 4), cmd.parts.len);

            switch (cmd.parts[1]) {
                .variable => |v| try std.testing.expectEqualStrings("a", v),
                else => try std.testing.expect(false),
            }

            switch (cmd.parts[3]) {
                .variable => |v| try std.testing.expectEqualStrings("b", v),
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses backslash line continuation in quoted string" {
    // When using a quoted string that spans lines, the entire string is captured
    // The backslash and newline are preserved for the shell to handle
    const source =
        \\build(name) {
        \\  echo "$name \
        \\    is continued"
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    // The quoted string spans lines, so jake sees it as part of one command
    try std.testing.expectEqual(@as(usize, 1), func.body.len);

    switch (func.body[0]) {
        .shell_command => |cmd| {
            // Should contain the variable interpolation
            var has_variable = false;
            var has_continuation = false;
            for (cmd.parts) |part| {
                switch (part) {
                    .variable => |v| {
                        if (std.mem.eql(u8, v, "name")) has_variable = true;
                    },
                    .text => |t| {
                        // Check that the continuation text is preserved
                        if (std.mem.indexOf(u8, t, "is continued") != null) {
                            has_continuation = true;
                        }
                    },
                }
            }
            try std.testing.expect(has_variable);
            try std.testing.expect(has_continuation);
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses unquoted backslash continuation" {
    // Without quotes, each line is a separate command
    // The backslash at end of line is passed to shell for continuation
    const source =
        \\build() {
        \\  echo hello \
        \\  echo world
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    // Two separate lines = two separate shell commands
    try std.testing.expectEqual(@as(usize, 2), func.body.len);
}

test "parser parses multiple functions" {
    const source =
        \\build(arch) {
        \\  echo build
        \\}
        \\
        \\test(num) {
        \\  @build(arm64)
        \\  echo test
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), result.functions.count());
}

test "parser detects duplicate functions" {
    const source =
        \\build() { echo a }
        \\build() { echo b }
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(result.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), result.errors.items.len);
    try std.testing.expectEqual(ast.ParseError.Kind.duplicate_function, result.errors.items[0].kind);
}

test "parser parses simple match statement" {
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
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;
    try std.testing.expectEqual(@as(usize, 1), func.body.len);

    switch (func.body[0]) {
        .match_statement => |match_stmt| {
            try std.testing.expectEqualStrings("type", match_stmt.variable);
            try std.testing.expectEqual(@as(usize, 2), match_stmt.arms.len);
            try std.testing.expectEqual(@as(usize, 1), match_stmt.arms[0].values.len);
            try std.testing.expectEqualStrings("debug", match_stmt.arms[0].values[0]);
            try std.testing.expectEqualStrings("release", match_stmt.arms[1].values[0]);
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses match with multi-value arms" {
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
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;

    switch (func.body[0]) {
        .match_statement => |match_stmt| {
            try std.testing.expectEqual(@as(usize, 2), match_stmt.arms.len);
            // First arm has one value
            try std.testing.expectEqual(@as(usize, 1), match_stmt.arms[0].values.len);
            // Second arm has two values
            try std.testing.expectEqual(@as(usize, 2), match_stmt.arms[1].values.len);
            try std.testing.expectEqualStrings("release", match_stmt.arms[1].values[0]);
            try std.testing.expectEqualStrings("small", match_stmt.arms[1].values[1]);
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses nested match statements" {
    const source =
        \\build(arch: {arm64|x86_64}, type: {debug|release}) {
        \\  match $arch {
        \\    arm64: {
        \\      match $type {
        \\        debug: {
        \\          echo arm64-debug
        \\        }
        \\        release: {
        \\          echo arm64-release
        \\        }
        \\      }
        \\    }
        \\    x86_64: {
        \\      echo x86_64
        \\    }
        \\  }
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;

    // Outer match
    switch (func.body[0]) {
        .match_statement => |outer_match| {
            try std.testing.expectEqualStrings("arch", outer_match.variable);
            try std.testing.expectEqual(@as(usize, 2), outer_match.arms.len);
            // Inner match in first arm
            try std.testing.expectEqual(@as(usize, 1), outer_match.arms[0].body.len);
            switch (outer_match.arms[0].body[0]) {
                .match_statement => |inner_match| {
                    try std.testing.expectEqualStrings("type", inner_match.variable);
                    try std.testing.expectEqual(@as(usize, 2), inner_match.arms.len);
                },
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses match with @jake_call in body" {
    const source =
        \\setup() { echo setup }
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      @setup()
        \\    }
        \\    release: {
        \\      echo release
        \\    }
        \\  }
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;

    switch (func.body[0]) {
        .match_statement => |match_stmt| {
            switch (match_stmt.arms[0].body[0]) {
                .jake_call => |call| {
                    try std.testing.expectEqualStrings("setup", call.name);
                },
                else => try std.testing.expect(false),
            }
        },
        else => try std.testing.expect(false),
    }
}

test "parser parses match with shell commands in body" {
    const source =
        \\build(type: {debug|release}) {
        \\  match $type {
        \\    debug: {
        \\      echo building debug
        \\      zig build
        \\    }
        \\    release: {
        \\      zig build -Doptimize=ReleaseFast
        \\    }
        \\  }
        \\}
    ;
    var result = Parser.parse(std.testing.allocator, source);
    defer result.deinit();

    try std.testing.expect(!result.hasErrors());
    const func = result.functions.get("build").?;

    switch (func.body[0]) {
        .match_statement => |match_stmt| {
            // Debug arm has 2 commands
            try std.testing.expectEqual(@as(usize, 2), match_stmt.arms[0].body.len);
            // Release arm has 1 command
            try std.testing.expectEqual(@as(usize, 1), match_stmt.arms[1].body.len);
        },
        else => try std.testing.expect(false),
    }
}
