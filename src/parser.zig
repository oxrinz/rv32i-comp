const std = @import("std");
const tokens_script = @import("tokens.zig");
const Token = tokens_script.Token;
const TokenType = tokens_script.TokenType;
const c_ast = @import("ast/c.zig");

// zig whines without this
const ParserError = error{
    OutOfMemory,
    UnexpectedToken,
};

pub const Parser = struct {
    tokens: []const Token,
    cursor: usize,
    allocator: std.mem.Allocator,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .cursor = 0,
            .allocator = allocator,
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
        // ??????????????
        // ??????????????
        const expr_ptr = self.parse_expression(0) catch @panic("failed");
        std.debug.print("parsing exp: {}\n", .{expr_ptr});
        const expr = expr_ptr.*;
        self.allocator.destroy(expr_ptr);
        return .{
            .type = .RETURN,
            .exp = expr,
        };
    }

    fn parse_expression(self: *Parser, min_prec: i16) ParserError!*c_ast.Expression {
        var left = try self.parse_factor();

        while (self.cursor < self.tokens.len and
            tokens_script.is_binary_operator(self.tokens[self.cursor].type))
        {
            const curr_prec = self.precedence(self.tokens[self.cursor]);
            if (curr_prec < min_prec) break;

            const operator = self.parse_binop();
            self.cursor += 1;

            const right = try self.parse_expression(curr_prec + 1);

            const new_expr = try self.allocator.create(c_ast.Expression);
            new_expr.* = .{
                .binary = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                },
            };

            left = new_expr;
        }

        return left;
    }

    fn parse_factor(self: *Parser) ParserError!*c_ast.Expression {
        const expr = try self.allocator.create(c_ast.Expression);

        switch (self.tokens[self.cursor].type) {
            .NUMBER => {
                expr.* = .{
                    .factor = .{
                        .constant = self.tokens[self.cursor].literal.?.number,
                    },
                };
                self.cursor += 1;
            },
            .LEFT_PAREN => {
                self.cursor += 1;
                const inner_expr = try self.parse_expression(0);
                self.expect(.RIGHT_PAREN);
                self.cursor += 1;

                expr.* = .{
                    .factor = .{
                        .expression = inner_expr,
                    },
                };
            },
            else => unreachable,
        }

        return expr;
    }

    fn parse_binop(self: *Parser) c_ast.BinaryOperator {
        switch (self.tokens[self.cursor].type) {
            .PLUS => return .Add,
            .MINUS => return .Subtract,
            .STAR => return .Multiply,
            .SLASH => return .Divide,
            .PERCENTAGE => return .Remainder,

            .AMPERSAND => return .Bitwise_AND,
            .PIPE => return .Bitwise_OR,
            .CARET => return .Bitwise_XOR,
            .LEFT_SHIFT => return .Left_Shift,
            .RIGHT_SHIFT => return .Right_Shift,

            .LESS => return .Less,
            .LESS_EQUAL => return .Less_Or_Equal,
            .GREATER => return .Greater,
            .GREATER_EQUAL => return .Greater_Or_Equal,
            .EQUAL => return .Equal,
            .BANG_EQUAL => return .Not_Equal,
            .AMPERSAND_AMPERSAND => return .And,
            .PIPE_PIPE => return .Or,
            else => unreachable,
        }
    }

    fn precedence(self: *Parser, token: Token) i16 {
        _ = self;
        switch (token.type) {
            .PIPE_PIPE => return 5,
            .AMPERSAND_AMPERSAND => return 10,
            .EQUAL, .BANG_EQUAL => return 30,
            .LESS, .LESS_EQUAL, .GREATER, .GREATER_EQUAL => return 35,
            .LEFT_SHIFT, .RIGHT_SHIFT => return 48,
            .AMPERSAND => return 47,
            .CARET => return 46,
            .PIPE => return 45,
            .PLUS, .MINUS => return 45,
            .STAR, .SLASH, .PERCENTAGE => return 50,
            else => unreachable,
        }
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
