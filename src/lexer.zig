const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: u32,
        end: u32,
        line: u32,
        column: u32,
    };

    pub const Tag = enum {
        // Literals
        identifier,
        string_literal,
        multiline_string,

        // Punctuation
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        comma,
        colon,
        asterisk,
        at_sign,
        dollar,
        pipe,
        eq_eq, // ==
        bang_eq, // !=

        // Keywords
        kw_if,
        kw_else,

        // Whitespace/structure
        newline,
        eof,

        // Error
        invalid,
    };

    pub fn slice(self: Token, source: []const u8) []const u8 {
        return source[self.loc.start..self.loc.end];
    }
};

pub const Lexer = struct {
    source: []const u8,
    index: u32,
    line: u32,
    column: u32,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .index = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn next(self: *Lexer) Token {
        self.skipWhitespaceAndComments();

        const start = self.index;
        const start_line = self.line;
        const start_column = self.column;

        if (self.index >= self.source.len) {
            return self.makeToken(.eof, start, start_line, start_column);
        }

        const c = self.source[self.index];

        return switch (c) {
            '#' => self.skipComment(),
            '"' => self.scanString(start, start_line, start_column),
            '(' => self.singleChar(.l_paren, start, start_line, start_column),
            ')' => self.singleChar(.r_paren, start, start_line, start_column),
            '{' => self.singleChar(.l_brace, start, start_line, start_column),
            '}' => self.singleChar(.r_brace, start, start_line, start_column),
            ',' => self.singleChar(.comma, start, start_line, start_column),
            ':' => self.singleChar(.colon, start, start_line, start_column),
            '*' => self.singleChar(.asterisk, start, start_line, start_column),
            '@' => self.singleChar(.at_sign, start, start_line, start_column),
            '$' => self.singleChar(.dollar, start, start_line, start_column),
            '|' => self.singleChar(.pipe, start, start_line, start_column),
            '=' => self.scanEquals(start, start_line, start_column),
            '!' => self.scanBang(start, start_line, start_column),
            '\n' => self.scanNewline(start, start_line, start_column),
            else => {
                if (isIdentStart(c)) {
                    return self.scanIdentifierOrKeyword(start, start_line, start_column);
                }
                self.advance();
                return self.makeToken(.invalid, start, start_line, start_column);
            },
        };
    }

    fn skipWhitespaceAndComments(self: *Lexer) void {
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            switch (c) {
                ' ', '\t', '\r' => self.advance(),
                else => break,
            }
        }
    }

    fn skipComment(self: *Lexer) Token {
        // Skip until end of line
        while (self.index < self.source.len and self.source[self.index] != '\n') {
            self.advance();
        }
        // Return the next token (skip comments entirely in token stream)
        return self.next();
    }

    fn scanString(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        self.advance(); // consume first "

        // Check for multiline """
        if (self.index + 1 < self.source.len and
            self.source[self.index] == '"' and
            self.source[self.index + 1] == '"')
        {
            self.advance(); // consume second "
            self.advance(); // consume third "
            return self.scanMultilineString(start, start_line, start_column);
        }

        // Regular string
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            if (c == '"') {
                self.advance();
                return self.makeToken(.string_literal, start, start_line, start_column);
            }
            if (c == '\\' and self.index + 1 < self.source.len) {
                self.advance(); // skip backslash
                self.advance(); // skip escaped char
                continue;
            }
            if (c == '\n') {
                // Unterminated string
                return self.makeToken(.invalid, start, start_line, start_column);
            }
            self.advance();
        }

        // Unterminated string at EOF
        return self.makeToken(.invalid, start, start_line, start_column);
    }

    fn scanMultilineString(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        while (self.index < self.source.len) {
            if (self.index + 2 < self.source.len and
                self.source[self.index] == '"' and
                self.source[self.index + 1] == '"' and
                self.source[self.index + 2] == '"')
            {
                self.advance();
                self.advance();
                self.advance();
                return self.makeToken(.multiline_string, start, start_line, start_column);
            }
            if (self.source[self.index] == '\n') {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }

        // Unterminated multiline string
        return self.makeToken(.invalid, start, start_line, start_column);
    }

    fn scanIdentifierOrKeyword(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        while (self.index < self.source.len and isIdentChar(self.source[self.index])) {
            self.advance();
        }
        // Check for keywords
        const ident = self.source[start..self.index];
        const tag: Token.Tag = if (std.mem.eql(u8, ident, "if"))
            .kw_if
        else if (std.mem.eql(u8, ident, "else"))
            .kw_else
        else
            .identifier;
        return self.makeToken(tag, start, start_line, start_column);
    }

    fn scanEquals(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        self.advance(); // consume first =
        if (self.index < self.source.len and self.source[self.index] == '=') {
            self.advance(); // consume second =
            return self.makeToken(.eq_eq, start, start_line, start_column);
        }
        // Single = is invalid in jake
        return self.makeToken(.invalid, start, start_line, start_column);
    }

    fn scanBang(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        self.advance(); // consume !
        if (self.index < self.source.len and self.source[self.index] == '=') {
            self.advance(); // consume =
            return self.makeToken(.bang_eq, start, start_line, start_column);
        }
        // Single ! is invalid in jake
        return self.makeToken(.invalid, start, start_line, start_column);
    }

    fn scanNewline(self: *Lexer, start: u32, start_line: u32, start_column: u32) Token {
        self.advance();
        self.line += 1;
        self.column = 1;
        return self.makeToken(.newline, start, start_line, start_column);
    }

    fn singleChar(self: *Lexer, tag: Token.Tag, start: u32, start_line: u32, start_column: u32) Token {
        self.advance();
        return self.makeToken(tag, start, start_line, start_column);
    }

    fn advance(self: *Lexer) void {
        if (self.index < self.source.len) {
            self.index += 1;
            self.column += 1;
        }
    }

    fn makeToken(self: *Lexer, tag: Token.Tag, start: u32, start_line: u32, start_column: u32) Token {
        return Token{
            .tag = tag,
            .loc = .{
                .start = start,
                .end = self.index,
                .line = start_line,
                .column = start_column,
            },
        };
    }
};

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9') or c == '-';
}

// Tests
test "lexer tokenizes simple function" {
    const source = "build(arch) {\n  cmd(\"echo $arch\")\n}";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // build
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // arch
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.newline, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // cmd
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.string_literal, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.newline, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer handles multiline strings" {
    const source =
        \\cmd("""
        \\  line 1
        \\  line 2
        \\""")
    ;
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // cmd
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.multiline_string, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
}

test "lexer handles new parameter syntax with colon and asterisk" {
    const source = "test(arch: {*single|lb|k8s})";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // test
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // arch
    try std.testing.expectEqual(Token.Tag.colon, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.asterisk, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // single
    try std.testing.expectEqual(Token.Tag.pipe, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // lb
    try std.testing.expectEqual(Token.Tag.pipe, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // k8s
    try std.testing.expectEqual(Token.Tag.r_brace, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
}

test "lexer handles at sign for jake function calls" {
    const source = "@build($arch)";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.at_sign, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // build
    try std.testing.expectEqual(Token.Tag.l_paren, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.dollar, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // arch
    try std.testing.expectEqual(Token.Tag.r_paren, lexer.next().tag);
}

test "lexer skips comments" {
    const source = "# comment\nbuild()";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.newline, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // build
}

test "lexer tracks line and column" {
    const source = "build()\ntest()";
    var lexer = Lexer.init(source);

    const build_tok = lexer.next();
    try std.testing.expectEqual(@as(u32, 1), build_tok.loc.line);
    try std.testing.expectEqual(@as(u32, 1), build_tok.loc.column);

    _ = lexer.next(); // (
    _ = lexer.next(); // )
    _ = lexer.next(); // newline

    const test_tok = lexer.next();
    try std.testing.expectEqual(@as(u32, 2), test_tok.loc.line);
    try std.testing.expectEqual(@as(u32, 1), test_tok.loc.column);
}

test "lexer handles dollar sign for variables" {
    const source = "$arch";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.dollar, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag);
}

test "lexer tokenizes if keyword" {
    const source = "if else";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.kw_if, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.kw_else, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer tokenizes == and !=" {
    const source = "== !=";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.eq_eq, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.bang_eq, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, lexer.next().tag);
}

test "lexer tokenizes if statement" {
    const source = "if $type == debug {";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.kw_if, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.dollar, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // type
    try std.testing.expectEqual(Token.Tag.eq_eq, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // debug
    try std.testing.expectEqual(Token.Tag.l_brace, lexer.next().tag);
}

test "lexer handles else if" {
    const source = "else if $x != y";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.kw_else, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.kw_if, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.dollar, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // x
    try std.testing.expectEqual(Token.Tag.bang_eq, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // y
}

test "lexer distinguishes if from identifier starting with if" {
    const source = "if ifdef iffy";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(Token.Tag.kw_if, lexer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // ifdef
    try std.testing.expectEqual(Token.Tag.identifier, lexer.next().tag); // iffy
}
