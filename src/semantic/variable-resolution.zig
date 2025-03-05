const std = @import("std");
const c_ast = @import("../ast/c.zig");

pub const VariableResolution = struct {
    allocator: std.mem.Allocator,
    map: std.StringHashMap([]const u8), // key: non unique-name. value: unique name
    counter: usize,

    pub fn init(allocator: std.mem.Allocator) VariableResolution {
        return .{
            .allocator = allocator,
            .map = std.StringHashMap([]const u8).init(allocator),
            .counter = 0,
        };
    }

    pub fn resolve(self: *VariableResolution, program: c_ast.Program) c_ast.Program {
        for (program.function.block_items) |*block_item| {
            switch (block_item.*) {
                .declaration => {
                    block_item.declaration = self.resolve_declaration(block_item.declaration);
                },
                .statement => {
                    block_item.statement = self.resolve_statement(block_item.statement);
                },
            }
        }

        return program;
    }

    pub fn resolve_declaration(self: *VariableResolution, declaration: c_ast.Declaration) c_ast.Declaration {
        var result = declaration;

        if (self.map.contains(result.identifier)) @panic("Duplicate variable declaration");

        const unique_name = std.fmt.allocPrint(self.allocator, "var_{d}", .{self.counter}) catch @panic("Failed to allocate memory");
        self.counter += 1;
        self.map.put(result.identifier, unique_name) catch @panic("Failed to allocate memory");

        result.identifier = unique_name;

        if (result.initial != null) {
            result.initial = self.resolve_exp(result.initial.?);
        }

        return result;
    }

    pub fn resolve_statement(self: *VariableResolution, statement: c_ast.Statement) c_ast.Statement {
        var result = statement;
        switch (result) {
            .exp => {
                result.exp = self.resolve_exp(result.exp);
            },
            .ret => {
                result.ret.exp = self.resolve_exp(result.ret.exp);
            },
        }
        return result;
    }

    // traverse expression and rename all variables. there must be a better way to do this
    pub fn resolve_exp(self: *VariableResolution, expression: c_ast.Expression) c_ast.Expression {
        var result = expression;
        switch (result) {
            .variable => {
                result.variable.identifier = self.map.get(result.variable.identifier) orelse @panic("Undefined variable");
            },
            .binary => {
                result.binary.left.* = self.resolve_exp(result.binary.left.*);
                result.binary.right.* = self.resolve_exp(result.binary.right.*);
            },
            .assignment => {
                if (result.assignment.left.* != .variable) @panic("Invalid lvalue");
                result.assignment.left.* = self.resolve_exp(result.assignment.left.*);
                result.assignment.right.* = self.resolve_exp(result.assignment.right.*);
            },
            .constant => {},
        }
        return result;
    }
};
