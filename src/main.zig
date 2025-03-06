const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const SemanticAnalysis = @import("semantic-analysis.zig").SemanticAnalysis;
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

    const assembly: []const u8 = generate(source, allocator, debug_value) catch @panic("Failed to generate assembly");

    const dirname = std.fs.path.dirname(file_path) orelse ".";
    const stem = std.fs.path.stem(file_path);

    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const output_path = try std.fmt.bufPrint(&path_buf, "{s}/{s}.asm", .{
        dirname,
        stem,
    });

    const out_file = try std.fs.cwd().createFile(
        output_path,
        .{},
    );
    defer out_file.close();

    try out_file.writeAll(assembly);
}

fn generate(input: []const u8, allocator: std.mem.Allocator, debug_value: i32) ![]const u8 {
    var lexer = Lexer.init(allocator, input);
    lexer.scan();

    if (debug_value == 1 and builtin.is_test == false) {
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

    if (debug_value == 1 and builtin.is_test == false) {
        std.debug.print("\n======== Program ========\n", .{});
        prettyprinter.printProgram(program_definition);
        std.debug.print("===========================\n", .{});
    }

    var semantic = SemanticAnalysis.init(allocator);
    const analyzed_program_definition = semantic.analyze(program_definition);

    if (debug_value == 1 and builtin.is_test == false) {
        std.debug.print("\n=== Semantic analysis ===\n", .{});
        prettyprinter.printProgram(analyzed_program_definition);
        std.debug.print("===========================\n", .{});
    }

    var generator = Generator.init(analyzed_program_definition, allocator);
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

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\add t1 t0 t1
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

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\slt t1 t1 t0
        \\xori t1 t1 1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "less" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 < 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\slt t1 t0 t1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "greater or equal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 >= 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\slt t1 t0 t1
        \\xori t1 t1 1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "greater" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 > 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\slt t1 t1 t0
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "equal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 == 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\sub t1 t0 t1
        \\sltiu t1 t1 1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "not equal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input =
        \\int main()
        \\{
        \\ return 2 != 6;
        \\}
    ;

    const actual = try generate(input, arena.allocator(), 0);
    const expected =
        \\lui t0 0
        \\addi t0 t0 2
        \\lui t1 0
        \\addi t1 t1 6
        \\sub t1 t0 t1
        \\sltiu t1 t1 1
        \\xori t1 t1 1
        \\
    ;

    try std.testing.expectEqualStrings(expected, actual);
}

test "or short circuit 1" {
    const input =
        \\int main()
        \\{
        \\    return 5 < 3 || 2 == 2 || 2 == 3;
        \\}
    ;

    try testCToAssembly(input, "../shared_tests/or_short_circuit_1.asm");
}

test "or short circuit 2" {
    const input =
        \\int main()
        \\{
        \\    return 3 > 3 || 19 != 19 || 4 <= 3 || 2 >= 2 || 2 != 2;
        \\}
    ;

    try testCToAssembly(input, "../shared_tests/or_short_circuit_2.asm");
}

test "and short circuit 1" {
    const input =
        \\int main()
        \\{
        \\    return 2 == 2 && 2 <= 1 && 5 > 3;
        \\}
    ;

    try testCToAssembly(input, "../shared_tests/and_short_circuit_1.asm");
}

test "variables 1" {
    const input =
        \\int main()
        \\{
        \\    int beh = 5 + 1;
        \\    int bah = beh - 1 * 2;
        \\    return bah + 1;
        \\}
        \\
    ;

    try testCToAssembly(input, "../shared_tests/variables_1.asm");
}

test "variables 2" {
    const input =
        \\int main()
        \\{
        \\    int beh = 5 + 1;
        \\    int bah = beh - 1 * 2;
        \\    int bumbam = beh + bah;
        \\    return bumbam + beh;
        \\}
        \\
    ;

    try testCToAssembly(input, "../shared_tests/variables_2.asm");
}

test "in place operators" {
    const input =
        \\int main()
        \\{
        \\    int beh = 10;
        \\    int baaah = 5;
        \\    beh -= baaah - 3;
        \\} 
        \\
    ;

    try testCToAssembly(input, "../shared_tests/in_place_operators.asm");
}

test "if 1" {
    const input =
        \\int main()
        \\{
        \\    int ab = 0;
        \\    if (ab != 0)
        \\        ab = 2;
        \\    else
        \\        ab = 19;
        \\}
        \\
    ;

    try testCToAssembly(input, "../shared_tests/if_1.asm");
}

test "if 2" {
    const input =
        \\int main()
        \\{
        \\    int ab = 0;
        \\    if (ab == 0)
        \\        if (ab != 0)
        \\            ab = 2;
        \\        else
        \\            ab = 7;
        \\    else
        \\        ab = 19;
        \\}
        \\
    ;

    try testCToAssembly(input, "../shared_tests/if_2.asm");
}

test "if 3" {
    const input =
        \\int main()
        \\{
        \\    int ab = 0;
        \\    if (ab != 0)
        \\        ab = 2;
        \\
        \\    ab = 62;
        \\}
        \\
    ;

    try testCToAssembly(input, "../shared_tests/if_3.asm");
}

fn testCToAssembly(input: []const u8, expected_path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const actual = try generate(input, arena.allocator(), 0);

    const expected = try std.fs.cwd().readFileAlloc(
        arena.allocator(),
        expected_path,
        1024 * 1024,
    );

    try std.testing.expectEqualStrings(expected, actual);
}
