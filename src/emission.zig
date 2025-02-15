const std = @import("std");
const asm_ast = @import("ast/asm.zig");

pub const Emitter = struct {
    program: asm_ast.Program,

    pub fn init(program: asm_ast.Program) Emitter {
        return .{ .program = program };
    }

    fn reg_to_string() []const u8 {}

    pub fn write(self: *Emitter, out_name: []const u8) !void {
        const dirname = std.fs.path.dirname(out_name) orelse ".";
        const stem = std.fs.path.stem(out_name);

        var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const output_path = try std.fmt.bufPrint(&path_buf, "{s}/{s}.asm", .{
            dirname,
            stem,
        });

        const file = try std.fs.cwd().createFile(
            output_path,
            .{},
        );
        defer file.close();

        for (self.program.function.instructions) |instruction| {
            switch (instruction) {
                .rtype => |r| {
                    try std.fmt.format(file.writer(), "{s} {s} {s} {s}\n", .{
                        r.instr.to_string(),
                        r.destination.to_string(),
                        r.source1.to_string(),
                        r.source2.to_string(),
                    });
                },
                .addi => |addi| {
                    const imm = @as(i32, @intCast(addi.imm));
                    try std.fmt.format(file.writer(), "addi {s} {s} {}\n", .{
                        addi.destination.to_string(),
                        addi.source.to_string(),
                        imm,
                    });
                },
                .lui => |lui| {
                    const imm = @as(i32, @intCast(lui.imm));
                    try std.fmt.format(file.writer(), "lui {s} {}\n", .{
                        lui.destination.to_string(),
                        imm,
                    });
                },
            }
        }
    }
};
