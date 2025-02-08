const std = @import("std");

pub const BinaryOperator = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,

    Bitwise_AND,
    Bitwise_OR,
    Bitwise_XOR,
    Left_Shift,
    Right_Shift,
};

pub const Binary = struct {
    operator: BinaryOperator,
    left: *Expression,
    right: *Expression,
};

pub const Factor = union(enum) {
    constant: i32,
    expression: *Expression,
};

pub const Expression = union(enum) {
    factor: Factor,
    binary: Binary,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |*b| {
                b.left.deinit(allocator);
                allocator.destroy(b.left);
                b.right.deinit(allocator);
                allocator.destroy(b.right);
            },
            .factor => |f| switch (f) {
                .constant => {},
                .expression => |e| {
                    e.deinit(allocator);
                    allocator.destroy(e);
                },
            },
        }
    }
};

pub const StatementType = union(enum) {
    RETURN,
};

pub const Statement = struct {
    type: StatementType,
    exp: Expression,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        self.exp.deinit(allocator);
    }
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    statement: Statement,

    pub fn deinit(self: *FunctionDefinition, allocator: std.mem.Allocator) void {
        self.statement.deinit(allocator);
    }
};

pub const Program = struct {
    function: FunctionDefinition,

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
    }
};
