const std = @import("std");
const lexer = @import("lexer.zig");

const ParserError = error{
    ExpectedExpression,
    ExpectedOpeningBrace,
    ExpectedClosingBrace,
    ExpectedClosingCurlyBrace,
    ExpectedSemicolon,
    ExpectedEqualsTo,
    ExpectedIdentifier,
    UnsupportedLHS,
} || std.mem.Allocator.Error;

//Grammar Precedence
// Program -> Declrations* EOF
// Declaration -> VariableDeclaration | Statement;//This is just to distinguish between places that accept variable declarations and that don't. And if statement without scope for example can't have variable declaration
// VariableDeclaration -> var IDENTIFIER ("=" expression)? ";"
// Statement -> Print Statement | ExpressionStmt | BlockStmt | ifStatement | loopStatement
// PrintStatement -> "print" Expression ";"
// ExpressionStmt -> Expression ";"
// BlockStmt -> { Statement* }
// ifStatemtn -> "if" "(" expression ")" Statement ("else" Statement)
// loopStatement -> ("while" "(" expression ") Statement) | ("for" "(" (VariableDeclaration|expressionStatement|";") (expression)? ";" (expression)? ") Statement)
// Expression -> Assignment
// Assignment -> IDENTIFIER = Logical_Or
// Logical_Or -> Logical_And ("or" Logical_And)?
// Logical_And -> Equality ("and" Equality)?
// Equality -> Comparison (("==" | "!=") Comparison)*
// Comparison -> Term (("<=" | "<" | ">" | ">=") Term)*
// Term -> Factor (("+" | "-") Factor)*
// Factor -> Unary (("/" | "*") Unary)*
// Unary -> ("-" | "!")Unary | Primary
// Primary -> (NUMBER | STRING | IDENTIFIER | "true" | "false" | "nil")

//TODO: Make these anon structs with some metaprogramming?
const variableDeclStruct = struct{
    name: []const u8,
    expr: ?*Expr,
};

const unaryStruct  = struct{
    operator : *lexer.Token,
    operand : *Expr,
};

const binaryStruct = struct{
    leftOperand : *Expr,
    operator : *lexer.Token,
    rightOperand : *Expr,
};

pub const assignmentStruct = struct{
    lhs : *Expr,
    rhs : *Expr,
}; 

pub const ifStatementStruct = struct{
    condition : *Expr,
    thenBlock : *Stmt,
    elseBlock : ?*Stmt,
};

pub const loopStatementStruct = struct{
    condition : *Expr,
    body : *Stmt,
};

pub const Stmt = union(enum){
    varDecl : *variableDeclStruct,
    printStmt : *Expr,
    exprStmt : *Expr,
    blockStmt : *std.ArrayList(*Stmt),
    ifStmt : *ifStatementStruct,
    loopStmt : *loopStatementStruct,
};

pub const Expr = union(enum){
    Literal : union(enum){
        number : f64,
        string : []const u8,
        nil : ?bool, //This is always null.
        true : bool,
        false : bool,
        identifier: []const u8,
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
    //TODO: These don't have to optional
    Unary : ?*unaryStruct,
    Binary : ?*binaryStruct,
    Grouping : ?*Expr,
    Assignment: *assignmentStruct,
    Condtional : *binaryStruct,
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
    
    pub fn parse(self: *Self) ParserError!*std.ArrayList(*Stmt){
        //TODO: Handle errors gracefully, synchronize
        const statements = try self.arena.allocator().create(std.ArrayList(*Stmt));
        statements.* = std.ArrayList(*Stmt).empty;
        while (!self.match(&.{.EOF})){
            const stmt = try self.declaration();
            try statements.append(self.arena.allocator(), stmt);
        }
        return statements;
    }

    fn declaration(self: *Self) ParserError!*Stmt{
        if (self.match(&.{.VAR})){
            return self.variableDecl();
        }
        return self.statement();
    }

    fn variableDecl(self: *Self) ParserError!*Stmt{
        if (!self.match(&.{.IDENTIFIER})){
            return error.ExpectedIdentifier;
        }
        const identifer = self.previous().lexeme;
        var expr :?*Expr = null;

        const stmt = try self.arena.allocator().create(Stmt);
        const variable = try self.arena.allocator().create(variableDeclStruct);
        variable.name = identifer;

        if (self.match(&.{.EQUALS})){
            expr = try self.expression();
        }
        variable.expr = expr;
        stmt.* = .{.varDecl = variable};
        if (!self.match(&.{.SEMICOLON})){
            return error.ExpectedSemicolon;
        }
        return stmt;
    }

    fn statement(self: *Self) ParserError!*Stmt{
        if (self.match(&.{.PRINT})){
            return self.printStatement();
        }
        if (self.match(&.{.LEFT_CURLY_PAREN})){
            return self.blockStatement();
        }
        if (self.match(&.{.IF})){
            return self.ifStatement();
        }
        if (self.match(&.{.WHILE})){
            return self.whileStatement();
        }
        if (self.match(&.{.FOR})){
            return self.forStatement();
        }
        return try self.expressionStatement();
    }

    fn printStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt); 
        var expr: *Expr = undefined;
        expr = try self.expression(); 
        if (!self.match(&.{.SEMICOLON})){
            return error.ExpectedSemicolon;
        }
        stmt.* = .{.printStmt = expr};
        return stmt;
    } 

    fn expressionStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt); 
        const expr = try self.expression();
        if (!self.match(&.{.SEMICOLON})){
            return error.ExpectedSemicolon;
        }
        stmt.* = .{.exprStmt = expr};
        return stmt;
    }

    fn blockStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt); 
        const statements = try self.arena.allocator().create(std.ArrayList(*Stmt));
        statements.* = std.ArrayList(*Stmt).empty;
        while (!self.match(&.{.RIGHT_CURLY_PAREN}) and !self.match(&.{.EOF})){
            const blockStmt = try self.declaration();
            try statements.append(self.arena.allocator(), blockStmt);
        }
        if (self.previous().type != .RIGHT_CURLY_PAREN){
            return error.ExpectedClosingCurlyBrace;
        }
        stmt.* = .{.blockStmt = statements};
        return stmt;
    }

    fn ifStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt); 
        if (!self.match(&.{.LEFT_PAREN})){
            return error.ExpectedOpeningBrace;
        }
        const condition = try self.expression();
        if (!self.match(&.{.RIGHT_PAREN})){
            return error.ExpectedClosingBrace;
        }
        const thenClause : *Stmt = try self.statement();
        var elseClause : ?*Stmt = null;
        if (self.match(&.{.ELSE})){
            elseClause = try self.statement();
        }
        const ifStruct = try self.arena.allocator().create(ifStatementStruct);
        ifStruct.* = ifStatementStruct{
            .condition = condition,
            .thenBlock = thenClause,
            .elseBlock = elseClause,
        };
        stmt.* = .{.ifStmt = ifStruct};
        return stmt;
    }

    fn whileStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt);
        if (!self.match(&.{.LEFT_PAREN})){
            return error.ExpectedOpeningBrace;
        }
        const conditon = try self.expression();
        if (!self.match(&.{.RIGHT_PAREN})){
            return error.ExpectedClosingBrace;
        }
        const body = try self.statement();
        const loopStatement = try self.arena.allocator().create(loopStatementStruct);
        loopStatement.* = loopStatementStruct{
            .body = body,
            .condition = conditon,
        };
        stmt.* = .{.loopStmt = loopStatement};
        return stmt;
    }

    fn forStatement(self: *Self) ParserError!*Stmt{
        const stmt = try self.arena.allocator().create(Stmt);
        if (!self.match(&.{.LEFT_PAREN})){
            return error.ExpectedOpeningBrace;
        }
        var initializer : ?*Stmt = null;
        if (self.match(&.{.VAR})){
            initializer = try self.variableDecl();
        }else if (self.peek().type != .SEMICOLON){
            initializer = try self.expressionStatement();
        }else if (!self.match(&.{.SEMICOLON})){
            return error.ExpectedSemicolon;
        }
        var conditon : ?*Expr = null;
        if (self.peek().type != .SEMICOLON){
            conditon = try self.expression();
        }else{
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Literal = .{.true = true}};
            conditon = expr;
        }
        if (!self.match(&.{.SEMICOLON})){
            return error.ExpectedSemicolon;
        }
        var increment : ?*Expr  = null;
        if (self.peek().type != .RIGHT_PAREN){
            increment = try self.expression();
        }
        if (!self.match(&.{.RIGHT_PAREN})){
            return error.ExpectedClosingBrace;
        }

        const bodyBlock = try self.arena.allocator().create(std.ArrayList(*Stmt));
        bodyBlock.* = std.ArrayList(*Stmt).empty;
        const body = try self.statement();
        try bodyBlock.append(self.arena.allocator(), body);
        if (increment) |inc|{
            const incStmt = try self.arena.allocator().create(Stmt);
            incStmt.* = .{.exprStmt = inc};
            try bodyBlock.append(self.arena.allocator(), incStmt);
        }
        const bodyBlockStmt = try self.arena.allocator().create(Stmt);
        bodyBlockStmt.* = .{.blockStmt = bodyBlock};
        const loop = try self.arena.allocator().create(loopStatementStruct);
        loop.* = loopStatementStruct{
            .body = bodyBlockStmt,
            .condition = conditon orelse unreachable,
        };
        const loopStatement = try self.arena.allocator().create(Stmt);
        loopStatement.* = .{.loopStmt = loop};

        const forBlock = try self.arena.allocator().create(std.ArrayList(*Stmt));
        forBlock.* = std.ArrayList(*Stmt).empty;
        if (initializer) |forInit|{
            try forBlock.append(self.arena.allocator(),forInit);
        }
        try forBlock.append(self.arena.allocator(), loopStatement);
        stmt.* = .{.blockStmt =  forBlock};
        return stmt;
    }

    fn expression(self: *Self) ParserError!*Expr{
        return try self.assignment();
    }

    fn assignment(self: *Self) ParserError!*Expr{
        const left = try self.logical_or();
        if (!self.match(&.{.EQUALS})){
            return left;
        }
        if (std.meta.activeTag(left.*) != .Literal and std.meta.activeTag(left.Literal) != .identifier){
            return error.UnsupportedLHS;
        }
        const right = try self.expression();

        const assignmentExpr = try self.arena.allocator().create(assignmentStruct);
        assignmentExpr.lhs = left;
        assignmentExpr.rhs = right;
        const expr = try self.arena.allocator().create(Expr);
        expr.* = .{.Assignment = assignmentExpr};
        return expr;
    }

    fn logical_or(self: *Self) ParserError!*Expr{
        var left = try self.logical_and();
        while (self.match(&.{.OR})){
            const operator = self.previous();
            const right = try self.logical_and();
            const binExpr = try self.arena.allocator().create(binaryStruct);
            binExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Condtional = binExpr};
            left = expr;
        }
        return left;
    }

    fn logical_and(self: *Self) ParserError!*Expr{
        var left = try self.equality();
        while (self.match(&.{.AND})){
            const operator = self.previous();
            const right = try self.logical_and();
            const binExpr = try self.arena.allocator().create(binaryStruct);
            binExpr.* = .{
                .leftOperand = left,
                .operator = operator,
                .rightOperand = right,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{.Condtional = binExpr};
            left = expr;
        }
        return left;
    }

    fn equality(self: *Self) ParserError!*Expr{
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

    fn term(self: *Self) ParserError!*Expr{
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

    fn factor(self: *Self) ParserError!*Expr{
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

    fn unary(self: *Self) ParserError!*Expr{
        if (self.match(&.{.MINUS,.BANG})){
            const operator = self.previous();
            const operand = try self.unary();
            const unaryExpr = try self.arena.allocator().create(unaryStruct);
            unaryExpr.* = .{
                .operand = operand, 
                .operator = operator,
            };
            const expr = try self.arena.allocator().create(Expr);
            expr.* = .{
                .Unary = unaryExpr
            };
            return expr;
        }
        return self.primary();
    }

    fn primary(self: *Self) ParserError!*Expr{
        if (self.match(&.{.NUMBER,.STRING,.TRUE,.FALSE,.NIL,.IDENTIFIER}) == true){
            const expr = try self.arena.allocator().create(Expr);
            const token = self.previous();
            switch (token.type){
                .NUMBER => expr.* = .{.Literal = .{.number = token.literal.?.number}},
                .STRING => expr.* = .{.Literal = .{.string = token.literal.?.string}},
                .TRUE => expr.* = .{.Literal = .{.true = true}},
                .FALSE => expr.* = .{.Literal = .{.false = false}},
                .NIL => expr.* = .{.Literal = .{.nil = null}},
                .IDENTIFIER => expr.* = .{.Literal = .{.identifier = token.lexeme}},
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

//TODO: Rewrite if necessary to debug AST
pub fn prettyPrint(expr: *Expr) void{
    switch (expr.*){
        .Literal => |l|{
            switch (l) {
                .number => |number| std.debug.print("{d}", .{number}),
                .string => |string| std.debug.print("{s}", .{string}),
                .true => |boolean| std.debug.print("{any}", .{boolean}),
                .false => |boolean| std.debug.print("{any}", .{boolean}),
                .nil =>  std.debug.print("nil", .{}),
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







