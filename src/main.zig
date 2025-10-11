const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len > 2){
        std.debug.print("Usage: ./zlox_twi <file.lox>. You can omit the file name in you wanna run a REPL", .{});
    }else if (args.len == 2){
        try runFile(allocator, args[1]);
    }else{
        try runREPL(allocator);
    }
}

fn runFile(allocator: std.mem.Allocator, filePath: []const u8) !void{
    const cwd = std.fs.cwd();
    const source = try cwd.readFileAlloc(allocator, filePath, std.math.maxInt(usize));
    defer allocator.free(source);
    try run(source);
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
        try run(source);
        allocatingWriter.clearRetainingCapacity();
        stdinIoReader.toss(1);
        std.debug.print("> ",.{});
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

fn run(source: []u8) !void{
    std.debug.print("{s}\n", .{source});
}
