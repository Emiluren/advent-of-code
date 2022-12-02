const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("1input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [10]u8 = undefined;
    var largest = [_]u32{0, 0, 0};
    var current: u32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len > 0) {
            current += try std.fmt.parseInt(u32, line, 10);
        } else {
            insert(&largest, current);
            current = 0;
        }
    }
    insert(&largest, current);
    std.debug.print("Part 1: {d}\n", .{largest[0]});
    std.debug.print("Part 2: {d}\n", .{largest[0] + largest[1] + largest[2]});
}

fn insert(largest: *[3]u32, current: u32) void {
    if (current > largest[0]) {
        largest[2] = largest[1];
        largest[1] = largest[0];
        largest[0] = current;
    } else if (current > largest[1]) {
        largest[2] = largest[1];
        largest[1] = current;
    } else if (current > largest[2]) {
        largest[2] = current;
    }
}
