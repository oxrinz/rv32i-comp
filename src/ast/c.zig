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

    Less,
    Less_Or_Equal,
    Greater,
    Greater_Or_Equal,
    Equal,
    Not_Equal,
    And,
    Or,

    pub fn getType(op: *const BinaryOperator) enum { ARITHMETIC, BITWISE, COMPARISON, SHORT_CIRCUIT } {
        switch (op.*) {
            .Add, .Subtract, .Multiply, .Divide, .Remainder => return .ARITHMETIC,
            .Bitwise_AND, .Bitwise_OR, .Bitwise_XOR, .Left_Shift, .Right_Shift => return .BITWISE,
            .Less, .Less_Or_Equal, .Greater, .Greater_Or_Equal, .Equal, .Not_Equal => return .COMPARISON,
            .And, .Or => return .SHORT_CIRCUIT,
        }
    }
};

pub const Binary = struct {
    operator: BinaryOperator,
    left: *Expression,
    right: *Expression,
};

pub const Variable = struct {
    identifier: []const u8,
};

pub const Assignment = struct {
    left: *Expression,
    right: *Expression,
};

pub const Expression = union(enum) {
    constant: i32,
    binary: Binary,
    variable: Variable,
    assignment: Assignment,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |*b| {
                b.left.deinit(allocator);
                allocator.destroy(b.left);
                b.right.deinit(allocator);
                allocator.destroy(b.right);
            },
            .assignment => |*a| {
                a.left.deinit(allocator);
                allocator.destroy(a.left);
                a.right.deinit(allocator);
                allocator.destroy(a.right);
            },
            else => {},
        }
    }
};

pub const Return = struct {
    exp: Expression,
};

pub const If = struct {
    condition: Expression,
    then: *Statement,
    else_: ?*Statement,
};

pub const Statement = union(enum) {
    ret: Return,
    exp: Expression,
    if_: If,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .ret => {
                self.ret.exp.deinit(allocator);
            },
            .exp => {
                self.exp.deinit(allocator);
            },
            .if_ => {
                self.if_.condition.deinit(allocator);
                self.if_.then.deinit(allocator);
                if (self.if_.else_) |else_stmt| {
                    else_stmt.deinit(allocator);
                }
            },
        }
    }
};

pub const Declaration = struct {
    identifier: []const u8,
    initial: ?Expression,
};

pub const BlockItem = union(enum) {
    statement: Statement,
    declaration: Declaration,
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    block_items: []BlockItem,

    pub fn deinit(self: *FunctionDefinition, allocator: std.mem.Allocator) void {
        for (self.block_items) |item| {
            item.statement.deinit(allocator);
        }
    }
};

pub const Program = struct {
    function: FunctionDefinition,

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
    }
};
