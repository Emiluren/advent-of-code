const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("4input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [50]u8 = undefined;
    var part1: u32 = 0;
    var part2: u32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var split_it = std.mem.split(u8, line, ",");
        const elf1 = try parse_elf(split_it.first());
        const elf2 = try parse_elf(split_it.rest());

        if ((elf1[0] <= elf2[0] and elf1[1] >= elf2[1]) or
                (elf2[0] <= elf1[0] and elf2[1] >= elf1[1])) {
            part1 += 1;
        }

        if ((elf1[0] <= elf2[0] and elf1[1] >= elf2[0]) or
                (elf2[0] <= elf1[0] and elf2[1] >= elf1[0])) {
            part2 += 1;
        }
    }
    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
}

fn parse_elf(elf: []const u8) ![2]u8 {
    var split_it = std.mem.split(u8, elf, "-");
    const low = try std.fmt.parseInt(u8, split_it.first(), 10);
    const high = try std.fmt.parseInt(u8, split_it.rest(), 10);
    return [_]u8{ low, high };
}
