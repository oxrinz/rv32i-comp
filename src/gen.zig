const std = @import("std");
const c_ast = @import("ast/c.zig");
const asm_ast = @import("ast/asm.zig");

pub const Generator = struct {
    program: c_ast.Program,
    next_temp_reg: usize,
    instruction_buffer: std.ArrayList(asm_ast.Instruction),

    pub fn init(program: c_ast.Program, allocator: std.mem.Allocator) Generator {
        return .{
            .program = program,
            .next_temp_reg = 0,
            .instruction_buffer = std.ArrayList(asm_ast.Instruction).init(allocator),
        };
    }

    fn append_rtype(self: *Generator, instr: asm_ast.RType_Inst, source2: asm_ast.Reg) void {
        self.instruction_buffer.append(.{ .rtype = .{
            .instr = instr,
            .destination = asm_ast.Reg.t2,
            .source1 = asm_ast.Reg.t2,
            .source2 = source2,
        } }) catch @panic("Failed to append instruction");
    }

    fn getNextTempReg(self: *Generator) asm_ast.Reg {
        const reg = switch (self.next_temp_reg) {
            0 => asm_ast.Reg.t0,
            1 => asm_ast.Reg.t1,
            2 => asm_ast.Reg.t2,
            else => @panic("Out of temporary registers"),
        };
        self.next_temp_reg += 1;
        return reg;
    }

    fn releaseTempReg(self: *Generator) void {
        if (self.next_temp_reg > 0) {
            self.next_temp_reg -= 1;
        }
    }

    fn loadImmediate(self: *Generator, value: i32, dest_reg: asm_ast.Reg) !void {
        const unsigned_val: u32 = @bitCast(value);
        const upper_bits: u20 = @truncate(unsigned_val >> 12);
        const lower_bits: u12 = @truncate(unsigned_val);

        try self.instruction_buffer.append(.{
            .lui = .{
                .destination = dest_reg,
                .imm = upper_bits,
            },
        });

        try self.instruction_buffer.append(.{
            .addi = .{
                .destination = dest_reg,
                .source = dest_reg,
                .imm = lower_bits,
            },
        });
    }

    fn generateFactor(self: *Generator, factor: c_ast.Factor) error{OutOfMemory}!asm_ast.Reg {
        switch (factor) {
            .constant => |constant| {
                try self.loadImmediate(constant, asm_ast.Reg.t2);
                return asm_ast.Reg.t2;
            },
            .expression => |expr| {
                return try self.generateExpression(expr.*);
            },
        }
    }

    fn generateExpression(self: *Generator, exp: c_ast.Expression) error{OutOfMemory}!asm_ast.Reg {
        switch (exp) {
            .factor => |factor| {
                return try self.generateFactor(factor);
            },
            .binary => |binary| {
                if (binary.right.* == .factor and binary.right.*.factor == .constant) {
                    const temp_reg = self.getNextTempReg();
                    try self.loadImmediate(binary.right.*.factor.constant, temp_reg);

                    _ = try self.generateExpression(binary.left.*);

                    switch (binary.operator) {
                        .Add => {
                            self.append_rtype(.ADD, temp_reg);
                        },
                        .Subtract => {
                            self.append_rtype(.SUB, temp_reg);
                        },
                        .Multiply => {
                            self.append_rtype(.MUL, temp_reg);
                        },
                        .Divide => {
                            self.append_rtype(.DIV, temp_reg);
                        },
                        .Bitwise_AND => {
                            self.append_rtype(.AND, temp_reg);
                        },
                        .Bitwise_OR => {
                            self.append_rtype(.OR, temp_reg);
                        },
                        .Bitwise_XOR => {
                            self.append_rtype(.XOR, temp_reg);
                        },
                        .Left_Shift => {
                            self.append_rtype(.SLL, temp_reg);
                        },
                        .Right_Shift => {
                            self.append_rtype(.SRL, temp_reg);
                        },
                        else => @panic("Unsupported binary operator"),
                    }
                    self.releaseTempReg();
                    return asm_ast.Reg.t2;
                }

                _ = try self.generateExpression(binary.right.*);

                const right_reg = self.getNextTempReg();
                try self.instruction_buffer.append(.{
                    .rtype = .{
                        .instr = .ADD,
                        .destination = right_reg,
                        .source1 = asm_ast.Reg.t2,
                        .source2 = asm_ast.Reg.zero,
                    },
                });

                _ = try self.generateExpression(binary.left.*);

                std.debug.print("binop: {}", .{binary.operator});

                switch (binary.operator) {
                    .Add => {
                        self.append_rtype(.ADD, right_reg);
                    },
                    .Subtract => {
                        self.append_rtype(.SUB, right_reg);
                    },
                    .Multiply => {
                        self.append_rtype(.MUL, right_reg);
                    },
                    .Divide => {
                        self.append_rtype(.DIV, right_reg);
                    },
                    .Bitwise_AND => {
                        self.append_rtype(.AND, right_reg);
                    },
                    .Bitwise_OR => {
                        self.append_rtype(.OR, right_reg);
                    },
                    .Bitwise_XOR => {
                        self.append_rtype(.XOR, right_reg);
                    },
                    .Left_Shift => {
                        self.append_rtype(.SLL, right_reg);
                    },
                    .Right_Shift => {
                        self.append_rtype(.SRL, right_reg);
                    },
                    else => @panic("Unsupported binary operator"),
                }

                self.releaseTempReg();
                return asm_ast.Reg.t2;
            },
        }
    }

    pub fn generate(self: *Generator) !asm_ast.Program {
        if (self.program.function.statement.type == .RETURN) {
            const result_reg = try self.generateExpression(self.program.function.statement.exp);
            std.debug.assert(result_reg == .t2);

            return .{
                .function = .{
                    .identifier = self.program.function.identifier,
                    .instructions = try self.instruction_buffer.toOwnedSlice(),
                },
            };
        }

        @panic("Only return statements are supported");
    }
};
