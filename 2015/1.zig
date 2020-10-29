const std = @import("std");

fn read_input(allocator: *std.mem.Allocator) ![]u8 {
    var input_file = try std.fs.cwd().openFile("1input", .{});
    defer input_file.close();

    const in_stream = &input_file.inStream();
    return try in_stream.readAllAlloc(allocator, 8000);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

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

    const stdout = std.io.getStdOut().outStream();
    try stdout.print("Part 1: {}\nPart 2: {}\n", .{ floor, position });
}
