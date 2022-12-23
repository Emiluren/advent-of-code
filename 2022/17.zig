const std = @import("std");

const input = @embedFile("17input");
//const input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

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
    parse_rock([_][]const u8{
        "####",
        "",
        "",
        "",
    }),
    parse_rock([_][]const u8{
        ".#.",
        "###",
        ".#.",
        "",
    }),
    parse_rock([_][]const u8{
        "###",
        "..#",
        "..#",
        "",
    }),
    parse_rock([_][]const u8{
        "#",
        "#",
        "#",
        "#",
    }),
    parse_rock([_][]const u8{
        "##",
        "##",
        "",
        "",
    })
};

var board = std.bit_set.StaticBitSet(57_000).initEmpty();

pub fn main() !void {
    const start_time = std.time.microTimestamp();

    var i: usize = 0;
    var rock_count: usize = 0;
    var last_top: usize = 0;
    while (rock_count < 2022) : (rock_count += 1) {
        const rock = rocks[rock_count % rocks.len];
        var r = last_top + 3;
        var c: usize = 2;

        // if (rock_count < 12) {
        //     std.debug.print("new block\n", .{});
        //     print_board(r, c, rock);
        // }
        while (true) {
            const move = input[i % input.len];
            i += 1;

            if (move == '<') {
                // if (rock_count < 2) {
                //     std.debug.print("Jet of gas pushes rock left\n", .{});
                // }
                if (c > 0 and !collides(r, c-1, rock)) {
                    c -= 1;
                }
            } else {
                // if (rock_count < 2) {
                //     std.debug.print("Jet of gas pushes rock right\n", .{});
                // }
                if (!collides(r, c+1, rock)) {
                    c += 1;
                }
            }

            // if (rock_count < 2) {
            //     print_board(r, c, rock);
            // }

            if (r == 0 or collides(r-1, c, rock)) {
                for (rock) |rock_row, rr| {
                    for (rock_row) |tile, cc| {
                        if (!tile)
                            continue;

                        board.set((r+rr)*7 + c+cc);
                        last_top = std.math.max(last_top, r+rr+1);
                    }
                }
                break;
            }
            r -= 1;
            // if (rock_count < 2) {
            //     std.debug.print("Rock falls 1 unit\n", .{});
            //     print_board(r, c, rock);
            // }
        }
    }

    var part1: usize = last_top;
    var part2: usize = 0;

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}

fn board_has(r: usize, c: usize) bool {
    return board.isSet(r*7 + c);
}

fn collides(r: usize, c: usize, rock: Rock) bool {
    for (rock) |rock_row, rr| {
        for (rock_row) |tile, cc| {
            if (!tile)
                continue;

            if (c+cc >= 7)
                return true;

            //std.debug.print("{d}\n", .{(r+rr)*7 + c+cc});
            if (board_has(r+rr, c+cc))
                return true;
        }
    }
    return false;
}

fn print_board(r: usize, c: usize, rock: Rock) void {
    {
        var rr: usize = 3;
        while (true) : (rr -= 1) {
            std.debug.print("|", .{});
            var cc: usize = 0;
            while (cc < c) : (cc += 1) {
                std.debug.print(".", .{});
            }
            while (cc-c < 4) : (cc += 1) {
                if (rock[rr][cc-c]) {
                    std.debug.print("@", .{});
                } else {
                    std.debug.print(".", .{});
                }
            }
            while (cc < 7) : (cc += 1) {
                std.debug.print(".", .{});
            }
            std.debug.print("|\n", .{});
            if (rr == 0)
                break;
        }
    }

    if (r > 0) {
        var rr = r - 1;
        while (true) : (rr -= 1) {
            std.debug.print("|", .{});
            var cc: usize = 0;
            while (cc < 7) : (cc += 1) {
                if (board_has(rr, cc)) {
                    std.debug.print("#", .{});
                } else {
                    std.debug.print(".", .{});
                }
            }
            std.debug.print("|\n", .{});
            if (rr == 0)
                break;
        }
    }
    std.debug.print("+-------+\n\n", .{});
}
