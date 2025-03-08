const std = @import("std");
const c_ast = @import("../ast/c.zig");

const MapEntry = struct {
    new_name: []const u8,
    from_current_block: bool,
};

pub const VariableResolution = struct {
    allocator: std.mem.Allocator,
    counter: usize,

    pub fn init(allocator: std.mem.Allocator) VariableResolution {
        return .{
            .allocator = allocator,
            .counter = 0,
        };
    }

    pub fn resolve(self: *VariableResolution, program: c_ast.Program) c_ast.Program {
        var variable_map = std.StringHashMap(MapEntry).init(self.allocator);

        var new_program = program;

        const block = self.resolveBlock(new_program.function.block, &variable_map);
        new_program.function.block = block;
        return new_program;
    }

    fn resolveBlock(self: *VariableResolution, block: c_ast.Block, variable_map: *std.StringHashMap(MapEntry)) c_ast.Block {
        var new_block = block;
        new_block = new_block; // dig doesn't recognize that editing arrays with a pointer modifies an object, in this case new_block
        for (new_block.block_items) |*block_item| {
            switch (block_item.*) {
                .declaration => {
                    block_item.declaration = self.resolveDeclaration(block_item.declaration, variable_map);
                },
                .statement => {
                    block_item.statement = self.resolveStatement(block_item.statement, variable_map);
                },
            }
        }
        return new_block;
    }

    fn resolveDeclaration(self: *VariableResolution, declaration: c_ast.Declaration, variable_map: *std.StringHashMap(MapEntry)) c_ast.Declaration {
        var result = declaration;

        if (variable_map.get(result.identifier)) |entry| {
            if (entry.from_current_block) {
                @panic("Duplicate variable declaration");
            }
        }

        const unique_name = std.fmt.allocPrint(self.allocator, "var_{d}", .{self.counter}) catch @panic("Failed to allocate memory");
        self.counter += 1;

        variable_map.put(
            result.identifier,
            MapEntry{
                .from_current_block = true,
                .new_name = unique_name,
            },
        ) catch @panic("Failed to allocate memory");

        result.identifier = unique_name;

        if (result.initial != null) {
            result.initial = self.resolveExp(result.initial.?, variable_map);
        }

        return result;
    }

    fn cloneVariableMap(self: *VariableResolution, variable_map: *std.StringHashMap(MapEntry)) std.StringHashMap(MapEntry) {
        _ = self;
        var new_map = std.StringHashMap(MapEntry).init(variable_map.allocator);

        var iterator = variable_map.iterator();
        while (iterator.next()) |entry| {
            var new_entry = entry.value_ptr.*;
            new_entry.from_current_block = false;
            new_map.put(entry.key_ptr.*, new_entry) catch @panic("Failed to allocate memory");
        }

        return new_map;
    }

    fn resolveStatement(self: *VariableResolution, statement: c_ast.Statement, variable_map: *std.StringHashMap(MapEntry)) c_ast.Statement {
        var result = statement;
        switch (result) {
            .exp => {
                result.exp = self.resolveExp(result.exp, variable_map);
            },
            .ret => {
                result.ret.exp = self.resolveExp(result.ret.exp, variable_map);
            },
            .if_ => {
                var else_: ?*c_ast.Statement = null;
                if (result.if_.else_ != null) {
                    const resolved_else = self.resolveStatement(result.if_.else_.?.*, variable_map);
                    else_ = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                    else_.?.* = resolved_else;
                }

                const resolved_then = self.resolveStatement(result.if_.then.*, variable_map);
                const then_statement = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                then_statement.* = resolved_then;

                result.if_ = .{
                    .condition = self.resolveExp(result.if_.condition, variable_map),
                    .then = then_statement,
                    .else_ = else_,
                };
            },
            .compound => {
                var new_map = self.cloneVariableMap(variable_map);
                result.compound = self.resolveBlock(result.compound, &new_map);
            },
            .break_ => {},
            .continue_ => {},
            .do_while => |*do_while| {
                const body = self.resolveStatement(do_while.*.body.*, variable_map);
                const body_ptr = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                body_ptr.* = body;

                do_while.body = body_ptr;
                do_while.condition = self.resolveExp(do_while.*.condition, variable_map);
            },
            .for_ => |*for_| {
                switch (for_.init) {
                    .init_decl => {
                        for_.init.init_decl = self.resolveDeclaration(for_.init.init_decl, variable_map);
                    },
                    .init_exp => {
                        if (for_.init.init_exp != null) {
                            for_.init.init_exp = self.resolveExp(for_.init.init_exp.?, variable_map);
                        }
                    },
                }

                if (for_.condition != null) {
                    for_.condition = self.resolveExp(for_.*.condition.?, variable_map);
                }

                if (for_.post != null) {
                    for_.post = self.resolveExp(for_.*.post.?, variable_map);
                }

                const body = self.resolveStatement(for_.*.body.*, variable_map);
                const body_ptr = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                body_ptr.* = body;

                for_.body = body_ptr;
            },
            .while_ => |*while_| {
                const body = self.resolveStatement(while_.*.body.*, variable_map);
                const body_ptr = self.allocator.create(c_ast.Statement) catch @panic("Failed to allocate memory");
                body_ptr.* = body;

                while_.body = body_ptr;
                while_.condition = self.resolveExp(while_.*.condition, variable_map);
            },
        }
        return result;
    }

    // traverse expression and rename all variables. there must be a better way to do this
    fn resolveExp(self: *VariableResolution, expression: c_ast.Expression, variable_map: *std.StringHashMap(MapEntry)) c_ast.Expression {
        var result = expression;
        switch (result) {
            .variable => {
                result.variable.identifier = variable_map.get(result.variable.identifier).?.new_name;
            },
            .binary => {
                result.binary.left.* = self.resolveExp(result.binary.left.*, variable_map);
                result.binary.right.* = self.resolveExp(result.binary.right.*, variable_map);
            },
            .assignment => {
                if (result.assignment.left.* != .variable) @panic("Invalid lvalue");
                result.assignment.left.* = self.resolveExp(result.assignment.left.*, variable_map);
                result.assignment.right.* = self.resolveExp(result.assignment.right.*, variable_map);
            },
            .constant => {},
        }
        return result;
    }
};
