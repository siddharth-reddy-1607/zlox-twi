const std = @import("std");

pub const TokenType = enum{
    LEFT_PAREN, RIGHT_PAREN, LEFT_CURLY_PAREN, RIGHT_CURLY_PAREN,
    PLUS, MINUS, STAR,
    DOT, COMMA, SEMICOLON,

    SLASH, EQUALS, DOUBLE_EQUALS, BANG, BANG_EQUALS,
    LESS_THAN, LESS_THAN_EQUALS, GREATER_THAN, GREATER_THAN_EQUALS,

    IDENTIFIER,

    //Literals
    STRING, NUMBER, 

    //Keywords
    NIL, VAR, TRUE, FALSE,
    AND, OR, IF, ELSE, FOR, WHILE,
    FUN, RETURN, CLASS, THIS, SUPER,
    PRINT,

    EOF
};

const keywordMap = std.static_string_map.StaticStringMap(TokenType).initComptime(.{
    .{"nil", .NIL},
    .{"var", .VAR},
    .{"true", .TRUE},
    .{"false", .FALSE},
    .{"and", .AND},
    .{"or", .OR},
    .{"if", .IF},
    .{"else", .ELSE},
    .{"for", .FOR},
    .{"while", .WHILE},
    .{"fun", .FUN},
    .{"class", .CLASS},
    .{"this", .THIS},
    .{"super", .SUPER},
    .{"print", .PRINT},
});

pub const Literal = union(enum){
    number : f64,
    string : []const u8,
};

pub const Token = struct{
    const Self = @This();

    type : TokenType,
    lexeme : []const  u8,
    literal : ?Literal = null,
    line : usize,
    offset : usize,

    pub fn toString(self: Self, arena: *std.heap.ArenaAllocator) ![]const u8{
        var ret : []const u8 = undefined;
        if (self.literal) |lit|{
            switch (lit) {
                .string => |l| ret = try std.fmt.allocPrint(arena.allocator(),"{s} [{any}] [{s}]", .{self.lexeme, self.type, l}),
                .number => |l| ret = try std.fmt.allocPrint(arena.allocator(),"{s} [{any}] [{any}]", .{self.lexeme, self.type, l}),
            }
        }else{
            ret = try std.fmt.allocPrint(arena.allocator(),"{s} [{any}] [null]", .{self.lexeme, self.type});
        }
        return ret;
    }
};

pub const Scanner = struct{
    const Self = @This();
    tokens : std.ArrayList(*Token) = std.ArrayList(*Token){},
    start : usize = 0,
    current : usize = 0,
    line : usize = 0,
    source : ?[]const u8 = null,
    arena : *std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) !Scanner{
        const arena = try allocator.create(std.heap.ArenaAllocator);
        errdefer allocator.destroy(arena);
        arena.* = std.heap.ArenaAllocator.init(allocator);
        return .{
            .arena = arena,
        };
    }

    pub fn deinit(self: Self) void{
        const parentAllocator = self.arena.child_allocator;
        self.arena.deinit();
        parentAllocator.destroy(self.arena);
    }
    
    pub fn scan(self: *Self, source: []const u8) !void{
        self.source = source;
        while (true) {
            self.start = self.current;
            const character = self.advance() orelse break;
            switch (character){
                '(' => try self.addToken(.LEFT_PAREN),
                ')' => try self.addToken(.RIGHT_PAREN),
                '{' => try self.addToken(.LEFT_CURLY_PAREN),
                '}' => try self.addToken(.RIGHT_CURLY_PAREN),
                '+' => try self.addToken(.PLUS),
                '-' => try self.addToken(.MINUS),
                '*' => try self.addToken(.STAR),
                '.' => try self.addToken(.DOT),
                ',' =>  try self.addToken(.COMMA),
                ';' =>  try self.addToken(.SEMICOLON),
                '/' => {
                    if (self.match('/') orelse false){
                        while (self.peek() orelse null != '\n'){
                            _ = self.advance();
                        }
                    }else{
                        try self.addToken(.SLASH);
                    }
                },
                '!' => try if (self.match('=') orelse false) self.addToken(.BANG_EQUALS) else self.addToken(.BANG),
                '=' => try if (self.match('=') orelse false) self.addToken(.DOUBLE_EQUALS) else self.addToken(.EQUALS),
                '<' => try if (self.match('=') orelse false) self.addToken(.LESS_THAN_EQUALS) else self.addToken(.LESS_THAN),
                '>' => try if (self.match('=') orelse false) self.addToken(.GREATER_THAN_EQUALS) else self.addToken(.GREATER_THAN),
                ' ' => {},
                '\n' => {self.line += 1;},
                '\t' => {},
                '\r' => {},
                '"' => try self.addString(),
                '0'...'9' => try self.addNumber(),
                '_','a'...'z','A'...'Z' => try self.addIdentifier(),
                //TODO: Use error sets and gracefully handle errors
                else => {return error.UnknownToken;},
            }
        }
        try self.addToken(.EOF);
    }

    fn addToken(self: *Self, tokenType : TokenType) !void{
        const lexeme = try self.arena.allocator().alloc(u8, self.current - self.start);
        @memcpy(lexeme, self.source.?[self.start..self.current]);
        const token = try self.arena.allocator().create(Token);
        token.lexeme = lexeme;
        token.line = self.line;
        token.offset = self.start;
        token.type = tokenType;
        switch (tokenType){
            .STRING => {
                const literal = try self.arena.allocator().alloc(u8, (self.current - 1) - (self.start + 1)); // Strip opening and closing double quotes
                @memcpy(literal, self.source.?[self.start+1..self.current-1]);
                token.literal.? = .{.string = literal};
            },
            .NUMBER => {
                const literal = try std.fmt.parseFloat(f64, lexeme);
                token.literal.? = .{.number = literal}; 
            },
            else => {
                token.literal = null;
            }
        }
        try self.tokens.append(self.arena.allocator(), token);
    }

    fn addNumber(self: *Self) !void{
        //TODO: Check if we infact delegate these errors to the parser
        //What about 123. or 123abc => We treat them as two seperate token (123,DOT) and (123,IDENTIFIER) and we delegate these errors to the parser
        //Since spaces don't matter in lox
        while (self.isDigit(self.peek() orelse null)){
           _ = self.advance();
        }
        if (self.peek() orelse null == '.' and self.isDigit(self.peekNext() orelse null)){ 
            _ = self.advance(); //Consume the dot
            while (self.isDigit(self.peek() orelse null)){
                _ = self.advance();
            }
        }
        try self.addToken(.NUMBER);
    }

    fn addString(self: *Self) !void{
        while (self.peek() orelse null != '"'){
            self.line += if (self.peek() orelse null == '\n') 1 else 0;
            _ = self.advance();
        }
        //TODO: Handle unterminated string
        _ = self.advance(); //Consume the closing double qoutes
        try self.addToken(.STRING);
    }

    fn addIdentifier(self: *Self) !void{
       while (self.isAlphaUnderscore(self.peek() orelse null) or self.isDigit(self.peek() orelse null)){
           _ = self.advance();
       }
       if (keywordMap.get(self.source.?[self.start..self.current])) |keyword|{
           try self.addToken(keyword);
       }else{
           try self.addToken(.IDENTIFIER);
       }
    }

    fn isAtEnd(self: Self) bool{
        return self.current >= self.source.?.len;
    }

    fn advance(self: *Self) ?u8{
        if (self.isAtEnd()){
            return null;
        }
        const character = self.source.?[self.current];
        self.current += 1;
        return character;
    }

    fn peek(self: Self) ?u8{
        if (self.isAtEnd()){
            return null;
        }
        const character = self.source.?[self.current];
        return character;
    }

    fn peekNext(self: Self) ?u8{
        if (self.current + 1 >= self.source.?.len) return null;
        return self.source.?[self.current + 1];
    }

    fn match(self: *Self, check: u8) ?bool{
        if (self.isAtEnd()){
            return null;
        }
        const character = self.source.?[self.current];
        if (character != check){
            return false;
        }
        self.current += 1;
        return true;
    }

    fn isDigit(_: Self, character: ?u8) bool{
        if (character) |ch|{
            return ch >= '0' and ch <= '9';
        }
        return false;
    }

    fn isAlphaUnderscore(_: Self, character: ?u8) bool{
        if (character) |ch|{
            return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
        }
        return false;
    }
};
