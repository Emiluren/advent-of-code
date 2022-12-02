const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("2input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [10]u8 = undefined;
    var part1_score: u32 = 0;
    var part2_score: u32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const p1 = line[0] - 'A';
        part1_score += round_score(p1, line[2] - 'X');

        const p2 = switch (line[2]) {
            'X' => (p1 + 2) % 3,
            'Y' => p1,
            'Z' => (p1 + 1) % 3,
            else => 0,
        };
        part2_score += round_score(p1, p2);
    }
    std.debug.print("Part 1: {d}\n", .{part1_score});
    std.debug.print("Part 2: {d}\n", .{part2_score});
}

fn round_score(p1: u8, p2: u8) u8 {
    const outcome_score: u8 =
        if (p1 == p2) 3
        else if ((p1 + 1) % 3 == p2) 6
        else 0;
    return p2 + 1 + outcome_score;
}
