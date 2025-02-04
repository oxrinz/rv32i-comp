const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Generator = @import("gen.zig").Generator;
const Emitter = @import("emission.zig").Emitter;
const prettyprinter = @import("prettyprinter.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        std.debug.print("Usage: {s} <source.c>\n", .{args[0]});
        std.process.exit(1);
    }

    const file_path = args[1];

    if (!std.mem.endsWith(u8, file_path, ".c")) {
        std.debug.print("Error: File must have .c extension\n", .{});
        std.process.exit(1);
    }

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();

    const source = try allocator.alloc(u8, file_size);
    defer allocator.free(source);

    const bytes_read = try file.readAll(source);
    if (bytes_read != file_size) {
        std.debug.print("Error: Could not read entire file\n", .{});
        std.process.exit(1);
    }

    var lexer = Lexer.init(allocator, source);
    lexer.scan();
    defer lexer.deinit();

    var parser = Parser.init(lexer.tokens.items, allocator);
    var program_definition = parser.parse();
    defer program_definition.deinit(allocator);
    prettyprinter.printProgram(program_definition);

    // var generator = Generator.init(program_definition);
    // var program = try generator.generate(allocator);
    // defer program.deinit();

    // var emitter = Emitter.init(program);
    // try emitter.write(file_path);
}
