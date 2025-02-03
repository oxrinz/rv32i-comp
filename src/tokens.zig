const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    //
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    //
    IDENTIFIER,
    STRING,
    NUMBER,
    //
    INT,
    VOID,
    RETURN,
};

pub const Literal = union(enum) { string: []const u8, number: i32 };

pub const Token = struct {
    type: TokenType,
    literal: ?Literal,
    line: usize,

    pub fn init(token_type: TokenType, literal: ?Literal, line: usize) Token {
        return .{
            .type = token_type,
            .literal = literal,
            .line = line,
        };
    }
};
