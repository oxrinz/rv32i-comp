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
