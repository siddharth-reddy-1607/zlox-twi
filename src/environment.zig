const std = @import("std");
const eval = @import("eval.zig");

pub const EnvironmentError = error{
    UndefinedVariable,
} || std.mem.Allocator.Error;

pub const Environment = struct{
    const Self = @This();
    arena: *std.heap.ArenaAllocator,
    hashmap: *std.StringHashMap(*eval.Value),
    parentEnv: ?*Environment,

    pub fn init(allocator: std.mem.Allocator, parentEnv: ?*Environment) !Environment{
        const arena = try allocator.create(std.heap.ArenaAllocator);
        const hashmap = try allocator.create(std.StringHashMap(*eval.Value));
        errdefer {
            arena.deinit();
        }
        arena.* = std.heap.ArenaAllocator.init(allocator);
        hashmap.* = std.StringHashMap(*eval.Value).init(allocator);
        return Environment{
            .arena = arena,
            .hashmap = hashmap,
            .parentEnv = parentEnv,
        };
    }

    pub fn deinit(self: Self) void{
        const parentAllocator = self.arena.child_allocator;
        self.arena.deinit();
        self.hashmap.deinit();
        parentAllocator.destroy(self.arena);
        parentAllocator.destroy(self.hashmap);
    }

    pub fn create(self: *Self, identifier: []const u8, val: *eval.Value) !void{
        //TODO: Remove variable reclaration 
        try self.hashmap.put(identifier, val);
    }

    pub fn assign(self: ?*Self, identifier: []const u8, val: *eval.Value) !void{
        if (self) |env|{
            if (env.hashmap.get(identifier)) |_|{
                try env.hashmap.put(identifier, val);
                return;
            }
            try env.parentEnv.?.assign(identifier, val);
        }else{
            return EnvironmentError.UndefinedVariable;
        }
    }

    pub fn get(self: ?Self, identifier: []const u8) !*eval.Value{
        if (self == null) return EnvironmentError.UndefinedVariable;
        return self.?.hashmap.get(identifier) orelse self.?.parentEnv.?.get(identifier);
    }
};
