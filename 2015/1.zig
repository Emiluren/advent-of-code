const std = @import("std");
const fs = std.fs;

fn read_input(allocator: *std.mem.Allocator) ![]u8 {
    var input_file = try fs.File.openRead("1input");
    defer input_file.close();

    const in_stream = &input_file.inStream().stream;
    return try in_stream.readAllAlloc(allocator, 8000);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.direct_allocator);
    defer arena.deinit();

    const allocator = &arena.allocator;

    var input_string = try read_input(allocator);

    var floor: i32 = 0;
    for (input_string) |c| {
        if (c == '(') {
            floor += 1;
        } else if (c == ')') {
            floor -= 1;
        } else {
            unreachable;
        }
    }

    var stdout_file = try std.io.getStdOut();
    const stdout = &stdout_file.outStream().stream;
    try stdout.print("Part 1: {}\n", floor);
}
