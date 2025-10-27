const std = @import("std");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");



const Value = union(enum){
    number: f64,
    boolean: bool,
    string: []const u8,
    nil: ?bool,

    fn match(self: Value, tag: std.meta.Tag(Value)) bool{
        return std.meta.activeTag(self) == tag;
    }
};

pub const Evalutor = struct{
    const Self = @This();
    arena: *std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) !Evalutor{
       const arena = try allocator.create(std.heap.ArenaAllocator);
       errdefer(allocator.destroy(arena));
       arena.* = std.heap.ArenaAllocator.init(allocator);
       return Evalutor{
           .arena = arena,
       };
    }
    
    pub fn deinit(self: Self) void{
        const parentAllocator = self.arena.child_allocator;
        self.arena.deinit();
        parentAllocator.destroy(self.arena);
    }

    pub fn eval(self: Self, expr: *parser.Expr) !*Value{
       var val : *Value = undefined;
       switch (expr.*){
           .Literal => |l| {
               val = try self.arena.allocator().create(Value);
               switch(l){
                   .number => |number| val.* = .{.number = number},
                   .string => |string| val.* = .{.string = string},
                   .true => val.* = .{.boolean = true},
                   .false => val.* = .{.boolean = false},
                   .nil => |nil| {
                       val.* = .{.nil = if (nil) |_| unreachable else null};
                   }
               }
           },
           .Binary => |bin|{
               const leftVal = try self.eval(bin.?.leftOperand);
               const rightVal = try self.eval(bin.?.rightOperand);
               try isCompatibleBinaryOperation(leftVal, rightVal, bin.?.operator.type);
               val = try self.binaryOperation(leftVal, rightVal, bin.?.operator.type);
           },
           .Unary => |unary|{
               const operand = try self.eval(unary.?.operand);
               try isCompatibleUnaryOperation(operand, unary.?.operator.type);
               val = try self.unaryOperation(operand, unary.?.operator.type);
           },
           .Grouping => |grouping| val = try self.eval(grouping.?),
       }
       return val;
    }

    //TODO: Should be able to move compatability test functionality directly into operation
    fn isCompatibleUnaryOperation(operand: *Value, operator: lexer.TokenType) !void{
        switch (operator){
            .BANG => {
                //Can only use not operator on nil or false as they are the only falsy values in my Lox
                return switch (operand.*){
                    .number, .string => error.OperandTypeMismatchError,
                    .boolean, .nil => undefined,
                };
            },
            .MINUS => {
                return switch (operand.*){
                    .string, .boolean, .nil => error.OperandTypeMismatchError,
                    .number => undefined
                };
            },
            else => unreachable,
        }
    }

    fn unaryOperation(self: Self, operand: *Value, operator: lexer.TokenType) !*Value{
        const val = try self.arena.allocator().create(Value);
        switch (operator){
            .BANG => {
                //Can only use not operator on nil or false as they are the only falsy values in my Lox
                switch (operand.*){
                    .boolean => |b| val.* = .{.boolean = !b},
                    .nil => val.* = .{.boolean =true},
                    else => unreachable,
                }
            },
            .MINUS => {
                switch (operand.*){
                    .string, .boolean, .nil => unreachable,
                    .number => |number| val.* = .{.number = -number},
                }
            },
            else => unreachable,
        }
        return val;
    }

    fn isCompatibleBinaryOperation(leftVal: *Value, rightVal: *Value, operator: lexer.TokenType) !void{
        switch (operator) {
            .PLUS => {
                return switch (leftVal.*) {
                    .number => if (rightVal.match(.number)) undefined else error.OperandTypeMismatchError,
                    .string => if (rightVal.match(.string)) undefined else error.OperandTypeMismatchError,
                    else => error.OperandTypeMismatchError,
                };
            },
            .MINUS, .STAR, .SLASH => {
                return switch (leftVal.*) {
                    .number => if (rightVal.match(.number)) undefined else error.OperandTypeMismatchError,
                    else => error.OperandTypeMismatchError,
                };
            },
            //Equality comparision only on same types
            .DOUBLE_EQUALS, .BANG_EQUALS => {
                return switch (leftVal.*) {
                    .number => if (rightVal.match(.number)) undefined else error.OperandTypeMismatchError,
                    .string => if (rightVal.match(.string)) undefined else error.OperandTypeMismatchError,
                    .boolean => if (rightVal.match(.boolean)) undefined else error.OperandTypeMismatchError,
                    .nil => if (rightVal.match(.nil)) undefined else error.OperandTypeMismatchError,
                };
            },
            //Lesser and greater comparision only on numbers and strings
            .LESS_THAN_EQUALS, .LESS_THAN, .GREATER_THAN_EQUALS, .GREATER_THAN =>{
                return switch (leftVal.*){
                    .number => if (rightVal.match(.number)) undefined else error.OperandTypeMismatchError,
                    .string => if (rightVal.match(.string)) undefined else error.OperandTypeMismatchError,
                    else => error.OperandTypeMismatchError,
                };
            }, 
            else => unreachable,
        }
    }

    fn binaryOperation(self: Self, leftVal: *Value, rightVal: *Value, operator: lexer.TokenType) !*Value{
        const val = try self.arena.allocator().create(Value);
        switch (operator){
            .PLUS => {
                switch (leftVal.*){
                    .number => |lNum|{
                    const rNum = rightVal.*.number;
                    val.* = .{.number = lNum + rNum};
                    },
                    .string => |lStr|{
                        const rStr = rightVal.*.string;
                        const str = try self.arena.allocator().alloc(u8, lStr.len + rStr.len);
                        std.mem.copyForwards(u8, str[0..lStr.len], lStr);
                        std.mem.copyForwards(u8, str[lStr.len..], rStr);
                        val.* = .{.string = str};
                    },
                    else => unreachable,
                }
            },
            .MINUS, .STAR, .SLASH => |op| {
                const lNum = leftVal.*.number;
                const rNum = rightVal.*.number;
                switch (op){
                    .MINUS => val.* = .{.number = lNum - rNum},
                    .STAR => val.* = .{.number = lNum * rNum},
                    .SLASH => {
                        if (rNum == 0){
                            return error.DivisionByZero;
                        }
                        val.* = .{.number = lNum / rNum};
                    },
                    else => unreachable,
                }
            },
            .DOUBLE_EQUALS, .BANG_EQUALS => |eq|{
                switch (leftVal.*){
                    .string => {
                        const lStr = leftVal.*.string;
                        const rStr = rightVal.*.string;
                        val.* = .{.boolean = std.mem.eql(u8, lStr, rStr)};
                    },
                    .number => {
                        const lNum = leftVal.*.number;
                        const rNum = rightVal.*.number;
                        val.* = .{.boolean = lNum == rNum};
                    },
                    .boolean => {
                        const lBool = leftVal.*.boolean;
                        const rBool = rightVal.*.boolean;
                        val.* = .{.boolean = lBool == rBool};
                    },
                    .nil =>  val.* = .{.boolean = if (eq == .DOUBLE_EQUALS) true else false},
                }
            },
            .LESS_THAN, .LESS_THAN_EQUALS, .GREATER_THAN, .GREATER_THAN_EQUALS => |op| {
                switch (leftVal.*){
                    .string => {
                        const lStr = leftVal.*.string;
                        const rStr = rightVal.*.string;
                        switch (op){
                            .LESS_THAN => val.* = .{.boolean = std.mem.order(u8, lStr, rStr) == .lt},
                            .LESS_THAN_EQUALS => val.* = .{.boolean = std.mem.order(u8, lStr, rStr) == .lt or std.mem.order(u8, lStr, rStr) == .eq},
                            .GREATER_THAN => val.* = .{.boolean = std.mem.order(u8, lStr, rStr) == .gt},
                            .GREATER_THAN_EQUALS => val.* = .{.boolean = std.mem.order(u8, lStr, rStr) == .gt or std.mem.order(u8, lStr, rStr) == .eq},
                            else => unreachable,
                        }
                    },
                    .number => {
                        const lNum = leftVal.*.number;
                        const rNum = rightVal.*.number;
                        switch (op){
                            .LESS_THAN => val.* = .{.boolean = lNum < rNum},
                            .LESS_THAN_EQUALS => val.* = .{.boolean = lNum <= rNum},
                            .GREATER_THAN => val.* = .{.boolean = lNum > rNum},
                            .GREATER_THAN_EQUALS => val.* = .{.boolean = lNum >= rNum},
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
        return val;
    }
};

pub fn prettyPrint(val: *Value) void{
    switch (val.*){
        .number => |number| std.debug.print("{d}", .{number}),
        .string => |string| std.debug.print("{s}", .{string}),
        .boolean => |b| std.debug.print("{any}", .{b}),
        .nil => std.debug.print("nil", .{}),
    }
}
