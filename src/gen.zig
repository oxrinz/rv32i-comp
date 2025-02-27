const std = @import("std");
const c_ast = @import("ast/c.zig");
const asm_ast = @import("ast/asm.zig");

pub const Generator = struct {
    program: c_ast.Program,
    next_temp_reg: usize,
    instruction_buffer: std.ArrayList(asm_ast.Instruction),
    rd: asm_ast.Reg,
    rs1: asm_ast.Reg,
    rs2: asm_ast.Reg,
    immediate: i32,
    label: []const u8,

    pub fn init(program: c_ast.Program, allocator: std.mem.Allocator) Generator {
        return .{
            .program = program,
            .next_temp_reg = 0,
            .instruction_buffer = std.ArrayList(asm_ast.Instruction).init(allocator),
            .rd = asm_ast.Reg.t1,
            .rs1 = asm_ast.Reg.t1,
            .rs2 = asm_ast.Reg.t1,
            .immediate = 0,
            .label = "",
        };
    }

    fn appendInstr(self: *Generator, instr: asm_ast.InstructionType) void {
        const instr_converted = asm_ast.convert(instr);
        const instruction = switch (instr_converted) {
            .rtype => asm_ast.Instruction{
                .rtype = .{
                    .instr = instr_converted.rtype,
                    .destination = self.rd,
                    .source1 = self.rs1,
                    .source2 = self.rs2,
                },
            },
            .itype => asm_ast.Instruction{
                .itype = .{
                    .instr = instr_converted.itype,
                    .destination = self.rd,
                    .source = self.rs1,
                    .immediate = self.immediate,
                },
            },
            .btype => asm_ast.Instruction{ .btype = .{
                .instr = instr_converted.btype,
                .source1 = self.rs1,
                .source2 = self.rs2,
                .label = self.label,
            } },
        };
        self.instruction_buffer.append(instruction) catch @panic("Failed to append instruction");
    }

    fn loadImmediate(self: *Generator, value: i32) !void {
        const unsigned_val: u32 = @bitCast(value);
        const upper_bits: u20 = @truncate(unsigned_val >> 12);
        const lower_bits: u12 = @truncate(unsigned_val);

        try self.instruction_buffer.append(.{
            .lui = .{
                .destination = self.rd,
                .imm = upper_bits,
            },
        });

        try self.instruction_buffer.append(.{
            .itype = .{
                .instr = .ADDI,
                .destination = self.rd,
                .source = self.rd,
                .immediate = lower_bits,
            },
        });
    }

    fn generateFactor(self: *Generator, factor: c_ast.Factor) error{OutOfMemory}!void {
        switch (factor) {
            .constant => |constant| {
                try self.loadImmediate(constant);
            },
            .expression => |expr| {
                try self.generateExpression(expr.*);
            },
        }
    }

    fn appendOperator(self: *Generator, operator: c_ast.BinaryOperator) void {
        switch (operator) {
            .Add => {
                self.appendInstr(.ADD);
            },
            .Subtract => {
                self.appendInstr(.SUB);
            },
            .Multiply => {
                self.appendInstr(.MUL);
            },
            .Divide => {
                self.appendInstr(.DIV);
            },
            .Bitwise_AND => {
                self.appendInstr(.AND);
            },
            .Bitwise_OR => {
                self.appendInstr(.OR);
            },
            .Bitwise_XOR => {
                self.appendInstr(.XOR);
            },
            .Left_Shift => {
                self.appendInstr(.SLL);
            },
            .Right_Shift => {
                self.appendInstr(.SRL);
            },
            .Less => {
                self.rs1 = asm_ast.Reg.t0;
                self.rs2 = asm_ast.Reg.t1;
                self.appendInstr(.SLT);
            },
            .Less_Or_Equal => {
                self.rs1 = asm_ast.Reg.t1;
                self.rs2 = asm_ast.Reg.t0;
                self.appendInstr(.SLT);
                self.immediate = 1;
                self.rs1 = asm_ast.Reg.t1;
                self.appendInstr(.XORI);
                self.rs1 = asm_ast.Reg.t0;
            },
            .Greater => {
                self.rs1 = asm_ast.Reg.t1;
                self.rs2 = asm_ast.Reg.t0;
                self.appendInstr(.SLT);
            },
            .Greater_Or_Equal => {
                self.appendInstr(.SLT);
                self.rs1 = asm_ast.Reg.t0;
                self.rs2 = asm_ast.Reg.t1;
                self.immediate = 1;
                self.rs1 = asm_ast.Reg.t1;
                self.appendInstr(.XORI);
                self.rs1 = asm_ast.Reg.t0;
            },
            .Equal => {
                self.appendInstr(.SUB);
                self.immediate = 1;
                self.rs1 = asm_ast.Reg.t1;
                self.appendInstr(.SLTIU);
                self.rs1 = asm_ast.Reg.t0;
            },
            .Not_Equal => {
                self.appendInstr(.SUB);
                self.immediate = 1;
                self.rs1 = asm_ast.Reg.t1;
                self.appendInstr(.SLTIU);
                self.rs1 = asm_ast.Reg.t0;
                self.immediate = 1;
                self.rs1 = asm_ast.Reg.t1;
                self.appendInstr(.XORI);
                self.rs1 = asm_ast.Reg.t0;
            },
            .And, .Or => @panic("And and Or operators ran in appendOperator even though they have a separate function for generation. This shouldn't happen."),
            else => @panic("Unsupported binary operator"),
        }
    }

    // TODO: rename to generate and check for all comparison ops
    // figure out a way to do this without t3
    fn generateShortCircuitingExpression(self: *Generator, binary: c_ast.Binary, is_and: bool) error{OutOfMemory}!void {
        self.label = "end";
        switch (binary.operator) {
            .Equal => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) self.appendInstr(.BNE) else self.appendInstr(.BEQ);
            },
            .Not_Equal => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) self.appendInstr(.BEQ) else self.appendInstr(.BNE);
            },
            .Less => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) {
                    self.rs1 = .t1;
                    self.rs2 = .t0;
                } else {
                    self.rs1 = .t0;
                    self.rs2 = .t1;
                }

                self.appendInstr(.BLT);
            },
            .Less_Or_Equal => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) {
                    self.rs1 = .t0;
                    self.rs2 = .t1;
                } else {
                    self.rs1 = .t1;
                    self.rs2 = .t0;
                }

                self.appendInstr(.BGE);
            },
            .Greater => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) {
                    self.rs1 = .t0;
                    self.rs2 = .t1;
                } else {
                    self.rs1 = .t1;
                    self.rs2 = .t0;
                }

                self.appendInstr(.BLT);
            },
            .Greater_Or_Equal => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                if (is_and) {
                    self.rs1 = .t1;
                    self.rs2 = .t0;
                } else {
                    self.rs1 = .t0;
                    self.rs2 = .t1;
                }

                self.appendInstr(.BGE);
            },
            .And => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                self.appendInstr(.BNE);
            },
            .Or => {
                self.rd = .t1;
                try self.generateExpression(binary.right.*);

                self.rd = .t0;
                try self.generateExpression(binary.left.*);

                self.appendInstr(.BEQ);
            },
            else => @panic("Short circuiting expression not supported"),
        }
    }

    fn generateExpression(self: *Generator, exp: c_ast.Expression) error{OutOfMemory}!void {
        switch (exp) {
            .factor => |factor| {
                try self.generateFactor(factor);
            },
            .binary => |binary| {
                // TODO: useful for debugging, remove or incorporate in DEBUG mode properly
                // std.debug.print("left: {}\n right: {}\n", .{ exp.binary.left, exp.binary.right });

                const optype = binary.operator.getType();

                if (optype == .SHORT_CIRCUIT) {
                    if (binary.left.binary.operator.getType() == .SHORT_CIRCUIT or
                        (binary.left.* == .binary and binary.right.* == .binary and
                        binary.left.binary.operator.getType() == .COMPARISON and
                        binary.right.binary.operator.getType() == .COMPARISON))
                    {
                        if (binary.left.binary.operator.getType() == .SHORT_CIRCUIT) {
                            try self.generateExpression(binary.left.*);
                            try self.generateShortCircuitingExpression(binary.right.binary, binary.operator == .And);
                        } else {
                            self.rd = .t0;
                            try self.generateShortCircuitingExpression(binary.left.binary, binary.operator == .And);
                            self.rd = .t1;
                            try self.generateShortCircuitingExpression(binary.right.binary, binary.operator == .And);
                        }
                    } else {
                        @panic("Can't short-circuit non comparison operators");
                    }
                } else {
                    // check if right side expression is a constant. if it is, evaluate left side first (non constant)
                    // all expressions return t1. t0 is used for internal calculations. in other words, all right side expressions return t1, and left side return t0
                    const right_is_const = binary.right.* == .factor and binary.right.*.factor == .constant;
                    if (right_is_const) {
                        self.rd = .t0;
                        try self.generateExpression(binary.left.*);
                    }

                    self.rd = .t1;
                    try self.generateExpression(binary.right.*);

                    if (!right_is_const) {
                        self.rd = .t0;
                        try self.generateExpression(binary.left.*);
                    }

                    self.rd = .t1;
                    self.rs1 = .t0;
                    self.rs2 = .t1;
                    self.appendOperator(binary.operator);
                }
            },
        }
    }

    pub fn generate(self: *Generator) !asm_ast.Program {
        if (self.program.function.statement.type == .RETURN) {
            try self.generateExpression(self.program.function.statement.exp);

            // add label to end if branches exist
            var has_btype = false;
            for (self.instruction_buffer.items) |instr| {
                if (instr == .btype) {
                    has_btype = true;
                    break;
                }
            }

            if (has_btype) {
                try self.instruction_buffer.append(.{ .label = .{ .name = "end" } });
            }
            // --

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
