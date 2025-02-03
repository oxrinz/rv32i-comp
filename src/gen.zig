const std = @import("std");

const c_ast = @import("ast/c.zig");
const asm_ast = @import("ast/asm.zig");

pub const Generator = struct {
    program: c_ast.Program,

    pub fn init(program: c_ast.Program) Generator {
        return .{ .program = program };
    }

    pub fn generate(self: *Generator, allocator: std.mem.Allocator) !asm_ast.Program {
        const number: u32 = @intCast(self.program.function.statement.exp);
        const first_12_bits: u12 = @truncate(number);
        const last_20_bits: u20 = @truncate(number << (32 - 12));

        if (self.program.function.statement.type == .RETURN) {
            var instructions = try allocator.alloc(asm_ast.Instruction, 2);

            instructions[0] = .{
                .lui = .{
                    .destination = try allocator.dupe(u8, "t0"),
                    .imm = last_20_bits,
                },
            };
            instructions[1] = .{
                .addi = .{
                    .destination = try allocator.dupe(u8, "t0"),
                    .source = try allocator.dupe(u8, "t0"),
                    .imm = first_12_bits,
                },
            };

            return .{
                .allocator = allocator,
                .function = .{
                    .identifier = try allocator.dupe(u8, self.program.function.identifier),
                    .instructions = instructions,
                },
            };
        }
    }
};
