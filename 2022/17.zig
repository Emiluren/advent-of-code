const std = @import("std");

//const input = @embedFile("17input");
const input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

const Rock = [4][4]bool;

fn parse_rock(lines: [4][]const u8) Rock {
    var rock: Rock = undefined;
    var r: usize = 0;
    while (r < 4) : (r += 1) {
        var c: usize = 0;
        while (c < 4) : (c += 1) {
            rock[r][c] = if (c < lines[r].len)
                lines[r][c] == '#'
            else
                false;
        }
    }
    return rock;
}

const rocks = [_]Rock{
    parse_rock(
        "####",
        "",
        "",
        "",
    ),
    parse_rock(
        ".#.",
        "###",
        ".#.",
        "",
    ),
    parse_rock(
        "#",
        "#",
        "#",
        "#",
    ),
    parse_rock(
        "##",
        "##",
        "",
        "",
    )
};

pub fn main() !void {
    const start_time = std.time.microTimestamp();

    var i: usize = 0;
    var rock_count: usize = 0;
    while (rock_count < 2022) : (rock_count += 1) {
        const rock = rocks[rock_count % rocks.len];
        const c = input[i % input.len];
    }

    var part1: usize = 0;
    var part2: usize = 0;

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}
