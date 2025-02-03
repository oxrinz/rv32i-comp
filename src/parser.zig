const std = @import("std");
const tokens_script = @import("tokens.zig");
const Token = tokens_script.Token;
const TokenType = tokens_script.TokenType;
const c_ast = @import("ast/c.zig");

pub const Parser = struct {
    tokens: []const Token,
    cursor: usize,

    pub fn init(tokens: []const Token) Parser {
        return Parser{
            .tokens = tokens,
            .cursor = 0,
        };
    }

    pub fn parse(self: *Parser) c_ast.Program {
        return .{ .function = self.parse_function() };
    }

    fn parse_function(self: *Parser) c_ast.FunctionDefinition {
        self.expect(.INT);
        self.cursor += 1;
        self.expect(.IDENTIFIER);

        const identifier = self.tokens[self.cursor].literal.?.string;

        self.cursor += 1;
        self.expect(.LEFT_PAREN);
        self.cursor += 1;
        self.expect(.RIGHT_PAREN);

        self.cursor += 1;
        self.expect(.LEFT_BRACE);
        self.cursor += 1;

        const statement = self.parse_statement();

        return .{ .identifier = identifier, .statement = statement };
    }

    fn parse_statement(self: *Parser) c_ast.Statement {
        self.expect(.RETURN);
        self.cursor += 1;
        self.expect(.NUMBER);

        const exp = self.tokens[self.cursor].literal.?.number;

        self.cursor += 1;

        return .{
            .type = .RETURN,
            .exp = exp,
        };
    }

    fn expect(self: *Parser, token_type: TokenType) void {
        if (self.tokens[self.cursor].type != token_type) {
            var buf: [128]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buf);
            const msg = std.fmt.allocPrint(fba.allocator(), "Syntax error at line {}. Expected token type {}. Got token type {}", .{
                self.tokens[self.cursor].line,
                token_type,
                self.tokens[self.cursor].type,
            }) catch "Syntax error";
            @panic(msg);
        }
    }
};
