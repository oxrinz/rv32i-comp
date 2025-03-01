const std = @import("std");
const c_ast = @import("ast/c.zig");

pub fn printExpression(exp: c_ast.Expression, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}", .{spaces[0..indent]});
    switch (exp) {
        .constant => |c| {
            std.debug.print("Constant: {}\n", .{c});
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
        .variable => |v| {
            std.debug.print("Variable: {s}\n", .{v.identifier});
        },
        .assignment => |a| {
            std.debug.print("Assignment:\n", .{});
            std.debug.print("{s}Left:\n", .{spaces[0 .. indent + 2]});
            printExpression(a.left.*, indent + 4);
            std.debug.print("{s}Right:\n", .{spaces[0 .. indent + 2]});
            printExpression(a.right.*, indent + 4);
        },
    }
}

pub fn printStatement(stmt: c_ast.Statement, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}", .{spaces[0..indent]});
    switch (stmt) {
        .ret => {
            std.debug.print("RETURN\n", .{});
            printExpression(stmt.ret.exp, indent + 2);
        },
        .exp => printExpression(stmt.exp, indent),
    }
}

pub fn printDeclaration(decl: c_ast.Declaration, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}Declaration: {s}\n", .{ spaces[0..indent], decl.identifier });
    if (decl.initial) |initial| {
        std.debug.print("{s}Initializer:\n", .{spaces[0 .. indent + 2]});
        printExpression(initial, indent + 4);
    }
}

pub fn printBlockItem(item: c_ast.BlockItem, indent: usize) void {
    switch (item) {
        .statement => |stmt| printStatement(stmt, indent),
        .declaration => |decl| printDeclaration(decl, indent),
    }
}

pub fn printFunction(func: c_ast.FunctionDefinition, indent: usize) void {
    const spaces = " " ** 64;
    std.debug.print("{s}Function: {s}\n", .{ spaces[0..indent], func.identifier });
    std.debug.print("{s}Body:\n", .{spaces[0..indent]});

    for (func.block_items) |item| {
        printBlockItem(item, indent + 2);
    }
}

pub fn printProgram(program: c_ast.Program) void {
    std.debug.print("Program:\n", .{});
    printFunction(program.function, 2);
}
