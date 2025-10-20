const std = @import("std");
const lexer = @import("lexer.zig");

const ParserError = error{
    ExpectedExpression,
    ExpectedClosingBrace,
} || std.mem.Allocator.Error;

//Grammar Precedence
// Expression -> Equality
// Equality -> Comparison ((== | !=) Comparison)*
// Comparison -> Term ((<= | < | > | >=) Term)*
// Term -> Factor ((+ | -) Factor)*
// Factor -> Unary ((/ | *) Unary)*
// Unary -> (- | !)Unary | Primary
// Primary -> (NUMBER | STRING | true | false | nil)

//TODO: Make these anon structs with some metaprogramming?
const unaryStruct  = struct{
    operator : *lexer.Token,
    operand : *Expr,
};

const binaryStruct = struct{
    leftOperand : *Expr,
    operator : *lexer.Token,
    rightOperand : *Expr,
};

const Expr = union(enum){
    Literal : union(enum){
        number : f64,
        string : []const u8,
        nil : ?bool, //This is always null.
        true : bool,
        false : bool,
    },
    // Unary : *struct{
    //     operator : *lexer.Token,
    //     operand : *Expr,
    // },
    // Binary : *struct{
    //     leftOperand : *Expr,
    //     operator : *lexer.Token,
    //     rightOperand : *Expr,
    // },
    Unary : ?*unaryStruct,
    Binary : ?*binaryStruct,
    Grouping : ?*Expr,
};

pub const Parser = struct{
    const Self = @This();
    arena : *std.heap.ArenaAllocator,
    tokens : *std.ArrayList(*lexer.Token),
    current : usize = 0,

    pub fn init(allocator: std.mem.Allocator, tokens: *std.ArrayList(*lexer.Token)) ParserError!Parser{
        const arena = try allocator.create(std.heap.ArenaAllocator);
        errdefer allocator.destroy(arena);
        arena.* = std.heap.ArenaAllocator.init(allocator);
        return .{
            .arena = arena,
            .tokens = tokens,
        };
    }

    pub fn deinit(self: Self) void{
        const parentAlloc = self.arena.child_allocator;
        self.arena.deinit();
        parentAlloc.destroy(self.arena);
    }
    
    pub fn parse(self: *Self) !*Expr{
        //TODO: Handle errors gracefully
        return try self.expression();
    }

    fn expression(self: *Self) !*Expr{
        return try self.equality();
    }

    fn equality(self: *Self) !*Expr{
        var left = try self.comparison();
        while (self.match(&.{.DOUBLE_EQUALS,.BANG_EQUALS})){
            const operator = self.previous();
            const right = try self.comparison();
            const binaryExpr = try self.arena.allocator().create(binaryStruct);
            binaryExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Binary = binaryExpr};
            left = expr;
        }
        return left;
    }
    
    fn comparison(self: *Self) !*Expr{
        var left = try self.term();
        while (self.match(&.{.LESS_THAN,.LESS_THAN_EQUALS,.GREATER_THAN,.GREATER_THAN_EQUALS})){
            const operator = self.previous();
            const right = try self.term();
            const binaryExpr = try self.arena.allocator().create(binaryStruct);
            binaryExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Binary = binaryExpr};
            left = expr;
        }
        return left;
    }

    fn term(self: *Self) !*Expr{
        var left = try self.factor();
        while (self.match(&.{.PLUS,.MINUS})){
            const operator = self.previous();
            const right = try self.factor();
            const binaryExpr = try self.arena.allocator().create(binaryStruct);
            binaryExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Binary = binaryExpr};
            left = expr;
        }
        return left;
    }

    fn factor(self: *Self) !*Expr{
        var left = try self.unary();
        while (self.match(&.{.SLASH,.STAR})){
            const operator = self.previous();
            const right = try self.unary();
            const binaryExpr = try self.arena.allocator().create(binaryStruct);
            binaryExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Binary = binaryExpr};
            left = expr;
        }
        return left;
    }

    fn unary(self: *Self) !*Expr{
        if (self.match(&.{.MINUS,.BANG})){
            const operator = self.previous();
            var operand = try self.unary();
            const unaryExpr = try self.arena.allocator().create(unaryStruct);
            unaryExpr.* = .{
                .operand = operand, 
                .operator = operator,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{
                .Unary = unaryExpr
            };
            operand = expr;
        }
        return self.primary();
    }

    fn primary(self: *Self) ParserError!*Expr{
        if (self.match(&.{.NUMBER,.STRING,.TRUE,.FALSE,.NIL}) == true){
            const expr = try self.arena.allocator().create(Expr);
            const token = self.previous();
            switch (token.type){
                .NUMBER => expr.* = .{.Literal = .{.number = token.literal.?.number}},
                .STRING => expr.* = .{.Literal = .{.string = token.literal.?.string}},
                .TRUE => expr.* = .{.Literal = .{.true = true}},
                .FALSE => expr.* = .{.Literal = .{.false = false}},
                .NIL => expr.* = .{.Literal = .{.nil = null}},
                else => unreachable,
            }
            return expr;
        }

        if (self.match(&.{.LEFT_PAREN})){
            const group = try self.expression();
            if (!self.match(&.{.RIGHT_PAREN})){
                return ParserError.ExpectedClosingBrace;
            }
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{ .Grouping = group };
            return expr;
        }
        //If nothing matches, it must be a expression
        return ParserError.ExpectedExpression;
    }

    fn advance(self: *Self) *lexer.Token{
        if (self.isAtEnd()){
            return self.tokens.items[self.current];
        }
        self.current += 1;
        return self.previous();
    }

    fn peek(self: *Self) *lexer.Token{
        return self.tokens.items[self.current];
    }

    fn match(self: *Self, tokens: []const lexer.TokenType) bool{
        for (tokens) |t|{
            if (self.peek().*.type == t){
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn previous(self: *Self) *lexer.Token{
        return self.tokens.items[self.current - 1];
    }

    fn isAtEnd(self: *Self) bool{
        return self.tokens.items[self.current].*.type == lexer.TokenType.EOF;
    }
};

pub fn prettyPrint(expr: *Expr) void{
    switch (expr.*){
        .Literal => |l|{
            switch (l) {
                .number => |number| std.debug.print("{d}", .{number}),
                .string => |string| std.debug.print("{s}", .{string}),
                .true => |boolean| std.debug.print("{any}", .{boolean}),
                .false => |boolean| std.debug.print("{any}", .{boolean}),
                .nil => |nil| std.debug.print("{any}", .{nil.?}),
            }
        }, 
        .Binary => |binary|{
            std.debug.print("(", .{});
            prettyPrint(binary.?.leftOperand);
            std.debug.print("{s}", .{binary.?.operator.lexeme});
            prettyPrint(binary.?.rightOperand);
            std.debug.print(")", .{});
        },
        .Unary => |unary|{
            std.debug.print("(", .{});
            std.debug.print("{s}", .{unary.?.operator.lexeme});
            prettyPrint(unary.?.operand);
            std.debug.print(")", .{});
        },
        .Grouping => |group|{
            std.debug.print("[", .{});
            prettyPrint(group.?);
            std.debug.print("]", .{});
        }
    }
}







