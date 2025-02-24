const std = @import("std");
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Generator = @import("gen.zig").Generator;
const Emitter = @import("emission.zig").Emitter;
const prettyprinter = @import("prettyprinter.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const debug_str = std.process.getEnvVarOwned(allocator, "DEBUG") catch "";
    defer if (debug_str.len > 0) allocator.free(debug_str);
    const debug_value = if (debug_str.len > 0)
        std.fmt.parseInt(u8, debug_str, 10) catch 0
    else
        0;

    const args = try std.process.argsAlloc(allocator);
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
    const bytes_read = try file.readAll(source);

    if (bytes_read != file_size) {
        std.debug.print("Error: Could not read entire file\n", .{});
        std.process.exit(1);
    }

    var lexer = Lexer.init(allocator, source);
    lexer.scan();

    if (debug_value == 1) {
        std.debug.print("\n======== Tokens ========\n", .{});
        for (lexer.tokens.items) |token| {
            std.debug.print("{s} '{?}' at line {d}\n", .{
                @tagName(token.type),
                token.literal,
                token.line,
            });
        }
        std.debug.print("========================\n", .{});
    }

    var parser = Parser.init(lexer.tokens.items, allocator);
    const program_definition = parser.parse();

    if (debug_value == 1) {
        std.debug.print("\n======== Program ========\n", .{});
        prettyprinter.printProgram(program_definition);
        std.debug.print("===========================\n", .{});
    }

    var generator = Generator.init(program_definition, allocator);
    const program = try generator.generate();

    var emitter = Emitter.init(program);
    try emitter.write(file_path, allocator);
}

fn generate(input: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var lexer = Lexer.init(allocator, input);
    lexer.scan();

    var parser = Parser.init(lexer.tokens.items, allocator);
    const program_definition = parser.parse();

    var generator = Generator.init(program_definition, allocator);
    const program = try generator.generate();

    var emitter = Emitter.init(program);

    return try emitter.getAssemblyString(allocator);
}

test "basic addition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 + 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator());
    const expected =
        \\lui t0 0
        \\addi t0 t0 6
        \\lui t2 0
        \\addi t2 t2 2
        \\add t2 t0 t2
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "less or equal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 <= 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator());
    const expected =
        \\lui t0 0
        \\addi t0 t0 6
        \\lui t2 0
        \\addi t2 t2 2
        \\slt t2 t0 t2
        \\xori t2 t0 1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}
