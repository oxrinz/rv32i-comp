const std = @import("std");
const tokens_script = @import("tokens.zig");
const Token = tokens_script.Token;
const TokenType = tokens_script.TokenType;
const c_ast = @import("ast/c.zig");

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
        const expr_ptr = self.parse_expression(0) catch @panic("failed");
        const expr = expr_ptr.*;
        self.allocator.destroy(expr_ptr);
        return .{
            .type = .RETURN,
            .exp = expr,
        };
    }

    fn parse_expression(self: *Parser, min_prec: i16) !*c_ast.Expression {
        self.expect(.NUMBER);
        var left = try self.parse_factor();

        while (if (tokens_script.is_binary_operator(self.tokens[self.cursor].type))
            self.precedence(self.tokens[self.cursor]) >= min_prec
        else
            false)
        {
            std.debug.print("rolling\n", .{});
            const operator = self.parse_binop();

            self.cursor += 1;

            const right = try self.parse_expression(self.precedence(self.tokens[self.cursor - 1]) + 1);

            const new_expr = try self.allocator.create(c_ast.Expression);
            new_expr.* = .{
                .binary = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                },
            };

            left = new_expr;
            self.cursor += 1;
        }

        return left;
    }

    fn parse_factor(self: *Parser) !*c_ast.Expression {
        const expr = try self.allocator.create(c_ast.Expression);

        switch (self.tokens[self.cursor].type) {
            .NUMBER => expr.* = .{
                .factor = .{
                    .constant = self.tokens[self.cursor].literal.?.number,
                },
            },
            else => unreachable,
        }
        self.cursor += 1;
        return expr;
    }

    fn parse_binop(self: *Parser) c_ast.BinaryOperator {
        switch (self.tokens[self.cursor].type) {
            .PLUS => return .Add,
            .MINUS => return .Divide,
            .STAR => return .Multiply,
            .SLASH => return .Divide,
            .PERCENTAGE => return .Remainder,
            else => unreachable,
        }
    }

    fn precedence(self: *Parser, token: Token) i16 {
        _ = self;
        switch (token.type) {
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
