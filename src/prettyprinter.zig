const std = @import("std");
const c_ast = @import("ast/c.zig");

pub fn printExpression(exp: c_ast.Expression, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}", .{spaces[0..indent]});

    switch (exp) {
        .factor => |f| {
            switch (f) {
                .constant => |c| std.debug.print("Constant: {}\n", .{c}),
                .expression => |e| printExpression(e.*, indent + 2),
            }
        },
        .binary => |b| {
            const op = switch (b.operator) {
                .Add => "+",
                .Subtract => "-",
                .Multiply => "*",
                .Divide => "/",
                .Remainder => "%",
                .Bitwise_AND => "&",
                .Bitwise_OR => "|",
                .Bitwise_XOR => "^",
                .Left_Shift => "<<",
                .Right_Shift => ">>",
                .Less => "<",
                .Less_Or_Equal => "<=",
                .Greater => ">",
                .Greater_Or_Equal => ">=",
                .Equal => "==",
                .Not_Equal => "!=",
                .And => "&&",
                .Or => "||",
            };
            std.debug.print("Binary Op: {s}\n", .{op});
            std.debug.print("{s}Left:\n", .{spaces[0 .. indent + 2]});
            printExpression(b.left.*, indent + 4);
            std.debug.print("{s}Right:\n", .{spaces[0 .. indent + 2]});
            printExpression(b.right.*, indent + 4);
        },
    }
}

pub fn printStatement(stmt: c_ast.Statement, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}", .{spaces[0..indent]});
    switch (stmt.type) {
        .RETURN => std.debug.print("RETURN\n", .{}),
    }
    printExpression(stmt.exp, indent + 2);
}

pub fn printFunction(func: c_ast.FunctionDefinition, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}Function: {s}\n", .{ spaces[0..indent], func.identifier });
    printStatement(func.statement, indent + 2);
}

pub fn printProgram(program: c_ast.Program) void {
    std.debug.print("Program:\n", .{});
    printFunction(program.function, 2);
}
