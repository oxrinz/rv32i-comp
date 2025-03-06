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
        return .{ .function = self.parseFunction() };
    }

    // TODO: can't start function with a left paren
    fn parseFunction(self: *Parser) c_ast.FunctionDefinition {
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

        var function_body = std.ArrayList(c_ast.BlockItem).init(self.allocator);

        while (self.tokens[self.cursor].type != .RIGHT_BRACE) {
            const block_item = self.parseBlockItem();
            function_body.append(block_item) catch @panic("Failed to allocate memory");
            self.cursor += 1;
        }

        return .{
            .identifier = identifier,
            .block_items = function_body.toOwnedSlice() catch @panic("Failed to allocate memory"),
        };
    }

    // the thing i was thinking about
    // the assignment expression expects 2 expressions
    // it's part of the lvalue thing, the validity of the first expression will be evaluated later
    fn parseBlockItem(self: *Parser) c_ast.BlockItem {
        switch (self.tokens[self.cursor].type) {
            .INT => {
                return .{ .declaration = self.parseDeclaration() };
            },
            else => return .{ .statement = self.parseStatement() },
        }
    }

    fn parseDeclaration(self: *Parser) c_ast.Declaration {
        self.expect(.INT);
        self.cursor += 1;
        self.expect(.IDENTIFIER);

        const identifier = self.tokens[self.cursor].literal.?.string;
        self.cursor += 1;

        if (self.tokens[self.cursor].type == .SEMICOLON) {
            return .{ .identifier = identifier, .initial = null };
        } else {
            self.expect(.EQUAL);
            self.cursor += 1;
            const expression = self.parseExpression(0) catch @panic("Failed to parse expression");

            return .{ .identifier = identifier, .initial = expression.* };
        }
    }

    // fn parseIf(self: *Parser) c_ast.If {}

    fn parseStatement(self: *Parser) c_ast.Statement {
        if (self.tokens[self.cursor].type == .RETURN) {
            self.cursor += 1;

            // ??????????????
            // ??????????????
            const expr_ptr = self.parseExpression(0) catch @panic("Failed to parse expression");
            const expr = expr_ptr.*;
            self.allocator.destroy(expr_ptr);
            return .{
                .ret = .{ .exp = expr },
            };
        } else if (self.tokens[self.cursor].type == .IF) {
            self.cursor += 1;
            self.expect(.LEFT_PAREN);
            self.cursor += 1;
            const condition = self.parseExpression(0) catch @panic("Failed to parse expression");
            self.expect(.RIGHT_PAREN);
            self.cursor += 1;

            const then = self.parseStatement();
            const then_ptr = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
            then_ptr.* = then;

            var else_ptr: ?*c_ast.Statement = null;
            if (self.tokens[self.cursor + 1].type == .ELSE) {
                self.cursor += 2;
                const else_ = self.parseStatement();
                else_ptr = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                else_ptr.?.* = else_;
            }

            return .{
                .if_ = .{
                    .condition = condition.*,
                    .then = then_ptr,
                    .else_ = else_ptr,
                },
            };
        } else {
            const expr_ptr = self.parseExpression(0) catch @panic("Failed to parse expression");
            const expr = expr_ptr.*;
            self.allocator.destroy(expr_ptr);
            return .{
                .exp = expr,
            };
        }
    }

    fn parseExpression(self: *Parser, min_prec: i16) ParserError!*c_ast.Expression {
        var left = try self.parseFactor();

        while (self.cursor < self.tokens.len and
            tokens_script.is_binary_operator(self.tokens[self.cursor].type) and
            self.precedence(self.tokens[self.cursor]) >= min_prec)
        {
            const curr_prec = self.precedence(self.tokens[self.cursor]);

            if (self.tokens[self.cursor].type == .EQUAL) {
                self.cursor += 1;

                const right = try self.parseExpression(curr_prec);

                const new_expr = try self.allocator.create(c_ast.Expression);
                new_expr.* = .{
                    .assignment = .{
                        .left = left,
                        .right = right,
                    },
                };

                left = new_expr;
            }
            // check inplace operators
            // in place operators can only be used at the start of a line. therefore only run if the token 2 positions ago is on a different line
            else if (tokens_script.is_in_place_starter(self.tokens[self.cursor].type) == true and self.tokens[self.cursor - 1].type == .IDENTIFIER and self.tokens[self.cursor - 2].line != self.tokens[self.cursor].line) {
                const operator = self.parseBinop();
                self.cursor += 1;
                self.expect(.EQUAL);
                self.cursor += 1;

                const variable_expr = try self.allocator.create(c_ast.Expression);
                variable_expr.* = .{
                    .variable = .{ .identifier = self.tokens[self.cursor - 3].literal.?.string },
                };

                const binary_expr = try self.allocator.create(c_ast.Expression);
                binary_expr.* = .{
                    .binary = .{
                        .operator = operator,
                        .left = variable_expr,
                        .right = try self.parseExpression(curr_prec),
                    },
                };

                const new_expr = try self.allocator.create(c_ast.Expression);
                new_expr.* = .{
                    .assignment = .{
                        .left = left,
                        .right = binary_expr,
                    },
                };

                left = new_expr;
            } else {
                const operator = self.parseBinop();
                self.cursor += 1;

                const right = try self.parseExpression(curr_prec + 1);

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
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParserError!*c_ast.Expression {
        var expr = try self.allocator.create(c_ast.Expression);

        switch (self.tokens[self.cursor].type) {
            .NUMBER => {
                expr.* = .{
                    .constant = self.tokens[self.cursor].literal.?.number,
                };
                self.cursor += 1;
            },
            .LEFT_PAREN => {
                self.cursor += 1;
                const inner_expr = try self.parseExpression(0);
                self.expect(.RIGHT_PAREN);
                self.cursor += 1;

                expr = inner_expr;
            },
            .IDENTIFIER => {
                expr.* = .{
                    .variable = .{
                        .identifier = self.tokens[self.cursor].literal.?.string,
                    },
                };
                self.cursor += 1;
            },
            else => {
                var buf: [128]u8 = undefined;
                var fba = std.heap.FixedBufferAllocator.init(&buf);
                const msg = std.fmt.allocPrint(fba.allocator(), "Syntax error at line {}. Expected one of the following: NUMBER, LEFT_PAREN, IDENTIFIER. Got token type {}", .{
                    self.tokens[self.cursor].line,
                    self.tokens[self.cursor].type,
                }) catch "Syntax error";
                @panic(msg);
            },
        }

        return expr;
    }

    fn parseBinop(self: *Parser) c_ast.BinaryOperator {
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
            .EQUAL_EQUAL => return .Equal,
            .BANG_EQUAL => return .Not_Equal,
            .AMPERSAND_AMPERSAND => return .And,
            .PIPE_PIPE => return .Or,
            else => unreachable,
        }
    }

    fn precedence(self: *Parser, token: Token) i16 {
        _ = self;
        switch (token.type) {
            .EQUAL => return 1,
            .PIPE_PIPE => return 5,
            .AMPERSAND_AMPERSAND => return 10,
            .EQUAL_EQUAL, .BANG_EQUAL => return 30,
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
