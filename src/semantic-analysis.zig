const std = @import("std");
const c_ast = @import("ast/c.zig");
const VariableResolution = @import("semantic/variable-resolution.zig").VariableResolution;

pub const SemanticAnalysis = struct {
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) SemanticAnalysis {
        return .{ .allocator = allocator };
    }

    pub fn analyze(self: *SemanticAnalysis, program: c_ast.Program) c_ast.Program {
        var variable_resolution = VariableResolution.init(self.allocator);

        return variable_resolution.resolve(program);
    }
};

// test "undefined variable" {
//     const allocator = std.testing.allocator;

//     const items = [_]c_ast.BlockItem{
//         .{
//             .statement = .{
//                 .exp = .{
//                     .variable = .{
//                         .identifier = "undefined_var",
//                     },
//                 },
//             },
//         },
//     };

//     const block_items = try allocator.dupe(c_ast.BlockItem, &items);
//     defer allocator.free(block_items);

//     const program = c_ast.Program{
//         .function = .{
//             .identifier = "test_function",
//             .block_items = block_items,
//         },
//     };

//     var var_resolution = VariableResolution.init(allocator);
//     defer var_resolution.map.deinit();

//     _ = var_resolution.resolve(program);

//     unreachable;
// }
