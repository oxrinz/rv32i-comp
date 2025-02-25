const std = @import("std");

pub const Reg = enum {
    zero,
    t0,
    t1,
    t2,
    a0,

    pub fn toString(self: Reg) []const u8 {
        return @tagName(self);
    }
};

const Addi = struct {
    source: Reg,
    destination: Reg,
    imm: u12,
};

const Lui = struct {
    destination: Reg,
    imm: u20,
};

pub const InstructionType = enum {
    ADD,
    SUB,
    XOR,
    OR,
    AND,
    SLL,
    SRL,
    SRA,
    SLT,
    SLTU,
    MUL,
    MULH,
    MULSU,
    MULU,
    DIV,
    DIVU,
    REM,
    REMU,

    ADDI,
    // SUB,
    XORI,
    // OR,
    // AND,
    // SLL,
    // SRL,
    // SRA,
    SLTI,
    SLTIU,
    // MUL,
    // MULH,
    // MULSU,
    // MULU,
    // DIV,
    // DIVU,
    // REM,
    // REMU,

};

pub const RType_Inst = enum {
    ADD,
    SUB,
    XOR,
    OR,
    AND,
    SLL,
    SRL,
    SRA,
    SLT,
    SLTU,
    MUL,
    MULH,
    MULSU,
    MULU,
    DIV,
    DIVU,
    REM,
    REMU,

    pub fn toString(self: RType_Inst) []const u8 {
        const str = @tagName(self);
        comptime var max_len = 0;
        inline for (@typeInfo(RType_Inst).Enum.fields) |field| {
            max_len = @max(max_len, field.name.len);
        }
        var buf: [max_len]u8 = undefined;
        return std.ascii.lowerString(&buf, str);
    }
};

pub const IType_Inst = enum {
    ADDI,
    // SUB,
    XORI,
    // OR,
    // AND,
    // SLL,
    // SRL,
    // SRA,
    SLTI,
    SLTIU,
    // MUL,
    // MULH,
    // MULSU,
    // MULU,
    // DIV,
    // DIVU,
    // REM,
    // REMU,

    pub fn toString(self: IType_Inst) []const u8 {
        const str = @tagName(self);
        comptime var max_len = 0;
        inline for (@typeInfo(RType_Inst).Enum.fields) |field| {
            max_len = @max(max_len, field.name.len);
        }
        var buf: [max_len]u8 = undefined;
        return std.ascii.lowerString(&buf, str);
    }
};

pub fn convert(instr: InstructionType) union(enum) { rtype: RType_Inst, itype: IType_Inst } {
    const instr_name = @tagName(instr);

    inline for (@typeInfo(RType_Inst).Enum.fields) |field| {
        if (std.mem.eql(u8, instr_name, field.name)) {
            return .{ .rtype = @field(RType_Inst, field.name) };
        }
    }

    inline for (@typeInfo(IType_Inst).Enum.fields) |field| {
        if (std.mem.eql(u8, instr_name, field.name)) {
            return .{ .itype = @field(IType_Inst, field.name) };
        }
    }

    unreachable;
}

// pub const InstructionType = union(enum) {
//     rtype: RType_Inst,
//     itype: IType_Inst,
// };

const RType = struct {
    instr: RType_Inst,
    source1: Reg,
    source2: Reg,
    destination: Reg,
};

const IType = struct {
    instr: IType_Inst,
    source: Reg,
    destination: Reg,
    immediate: i32,
};

pub const Instruction = union(enum) {
    rtype: RType,

    itype: IType,

    lui: Lui,
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    instructions: []Instruction,
};

pub const Program = struct {
    function: FunctionDefinition,
};
