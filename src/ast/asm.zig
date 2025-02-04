const std = @import("std");

const Lui = struct {
    destination: []const u8,
    imm: u20,
};

const Addi = struct {
    source: []const u8,
    destination: []const u8,
    imm: u12,
};

pub const Instruction = union(enum) {
    lui: Lui,
    addi: Addi,
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    instructions: []Instruction,
};

pub const Program = struct {
    function: FunctionDefinition,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Program) void {
        for (self.function.instructions) |instruction| {
            switch (instruction) {
                .lui => |lui| {
                    self.allocator.free(lui.destination);
                },
                .addi => |addi| {
                    self.allocator.free(addi.destination);
                    self.allocator.free(addi.source);
                },
            }
        }

        self.allocator.free(self.function.instructions);

        self.allocator.free(self.function.identifier);
    }
};
