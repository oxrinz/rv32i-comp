const std = @import("std");

pub const Reg = enum {
    zero,
    t0,
    t1,
    t2,
    a0,

    pub fn to_string(self: Reg) []const u8 {
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

    pub fn to_string(self: RType_Inst) []const u8 {
        const str = @tagName(self);
        comptime var max_len = 0;
        inline for (@typeInfo(RType_Inst).Enum.fields) |field| {
            max_len = @max(max_len, field.name.len);
        }
        var buf: [max_len]u8 = undefined;
        return std.ascii.lowerString(&buf, str);
    }
};

const RType = struct {
    instr: RType_Inst,
    source1: Reg,
    source2: Reg,
    destination: Reg,
};

pub const Instruction = union(enum) {
    rtype: RType,

    addi: Addi,

    lui: Lui,
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    instructions: []Instruction,
};

pub const Program = struct {
    function: FunctionDefinition,
};
