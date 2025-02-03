pub const StatementType = union(enum) {
    RETURN,
};

pub const Statement = struct {
    type: StatementType,
    exp: i32,
};

pub const FunctionDefinition = struct {
    identifier: []const u8,
    statement: Statement,
};

pub const Program = struct {
    function: FunctionDefinition,
};
