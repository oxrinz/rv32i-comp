const std = @import("std");
const c_ast = @import("ast/c.zig");
const IdentifierResolution = @import("semantic/identifier-resolution.zig").IdentifierResolution;
const LoopLabeling = @import("semantic/loop-labeling.zig").LoopLabeling;

pub const SemanticAnalysis = struct {
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) SemanticAnalysis {
        return .{ .allocator = allocator };
    }

    pub fn analyze(self: *SemanticAnalysis, program: c_ast.Program) c_ast.Program {
        var variable_resolution = IdentifierResolution.init(self.allocator);

        var loop_labeling = LoopLabeling.init(self.allocator);
        return loop_labeling.label(variable_resolution.resolve(program));
    }
};
