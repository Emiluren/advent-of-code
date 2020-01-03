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
    var found_basement = false;
    var position: i32 = 1;
    for (input_string) |c| {
        if (c == '(') {
            floor += 1;
        } else if (c == ')') {
            floor -= 1;
        } else {
            unreachable;
        }

        if (!found_basement) {
            if (floor == -1) {
                found_basement = true;
            } else {
                position += 1;
            }
        }
    }

    var stdout_file = try std.io.getStdOut();
    const stdout = &stdout_file.outStream().stream;
    try stdout.print("Part 1: {}\nPart 2: {}\n", floor, position);
}
