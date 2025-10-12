const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arenaAllocator = arena.allocator();
    
    const args = try std.process.argsAlloc(arenaAllocator);
    defer std.process.argsFree(arenaAllocator, args);
    if (args.len > 2){
        std.debug.print("Usage: ./zlox_twi <file.lox>. You can omit the file name in you wanna run a REPL", .{});
    }else if (args.len == 2){
        try runFile(arenaAllocator, args[1]);
    }else{
        try runREPL(arenaAllocator);
    }
}

fn runFile(allocator: std.mem.Allocator, filePath: []const u8) !void{
    const cwd = std.fs.cwd();
    const source = try cwd.readFileAlloc(allocator, filePath, std.math.maxInt(usize));
    defer allocator.free(source);
    try run(source, allocator);
}

fn runREPL(allocator: std.mem.Allocator) !void{
    var readBuffer : [1024]u8 = undefined;
    var stdinReader = std.fs.File.stdin().reader(&readBuffer);
    var stdinIoReader = &stdinReader.interface;
    
    var allocatingWriter = std.Io.Writer.Allocating.init(allocator);
    defer allocatingWriter.deinit();

    std.debug.print("> ",.{});
    while (stdinIoReader.streamDelimiter(&allocatingWriter.writer, '\n')) |_|{
        const source = allocatingWriter.written();
        try run(source, allocator);
        allocatingWriter.clearRetainingCapacity();
        stdinIoReader.toss(1);
        std.debug.print("\n> ",.{});
    }else |err|{
        switch (err){
            error.EndOfStream => {
                std.debug.print("\nThank you for using zlox_twi REPL :)", .{});
                return;
            },
            else => std.debug.print("Error while reading stdin: {any}", .{err})
        }
    }
}

fn run(source: []u8, allocator: std.mem.Allocator) !void{
    var scanner = lexer.Scanner{
        .allocator = allocator
    };
    try scanner.scan(source);
    for (scanner.tokens.items) |token|{
        std.debug.print("{s},",.{try token.*.toString(scanner.allocator)});
    }
}
