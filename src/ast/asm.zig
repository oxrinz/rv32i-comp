const std = @import("std");

pub const Reg = enum {
    zero,
    t0,
    t1,
    a0,

    pub fn to_string(self: Reg) []const u8 {
        return @tagName(self);
    }
};

const Add = struct {
    source1: Reg,
    source2: Reg,
    destination: Reg,
};

const Sub = struct {
    source1: Reg,
    source2: Reg,
    destination: Reg,
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

pub const Instruction = union(enum) {
    add: Add,
    sub: Sub,

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
