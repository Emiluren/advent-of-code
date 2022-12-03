const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("3input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [50]u8 = undefined;
    var part1: u32 = 0;
    outer: while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const size = line.len / 2;
        for (line[0..size]) |c1| {
            for (line[size..]) |c2| {
                if (c1 == c2) {
                    part1 += prio(c1);
                    continue :outer;
                }
            }
        }
    }
    std.debug.print("Part 1: {d}\n", .{part1});

    try file.seekTo(0);
    var buf2: [50]u8 = undefined;
    var buf3: [50]u8 = undefined;
    var part2: u32 = 0;
    outer: while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line1| {
        const line2 = try in_stream.readUntilDelimiterOrEof(&buf2, '\n') orelse "";
        const line3 = try in_stream.readUntilDelimiterOrEof(&buf3, '\n') orelse "";

        for (line1) |c1| {
            for (line2) |c2| {
                if (c1 != c2) continue;

                for (line3) |c3| {
                    if (c1 == c3) {
                        part2 += prio(c1);
                        continue :outer;
                    }
                }
            }
        }
    }
    std.debug.print("Part 2: {d}\n", .{part2});
}

fn prio(c: u8) u32 {
    return if (c >= 'a') c - 'a' + 1 else c - 'A' + 27;
}
