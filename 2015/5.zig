const std = @import("std");

const vowels = "aeiou";

pub fn main() !void {
    try part1();
    try part2();
}

fn part1() !void {
    var input_file = try std.fs.cwd().openFile("5input", .{});
    defer input_file.close();

    const reader = &input_file.reader();

    var nice_strings: usize = 0;
    while (true) {
        var buffer: [16]u8 = undefined;
        const read_bytes = try reader.read(&buffer);
        if (read_bytes < 16) {
            break;
        }
        try reader.skipBytes(1, .{});

        var last_letter: ?u8 = null;
        var vowel_count: u8 = 0;
        var has_double = false;
        var has_illegal_strings = false;
        for (buffer) |letter| {
            for (vowels) |v| {
                if (v == letter) {
                    vowel_count += 1;
                }
            }

            if (last_letter) |ll| {
                if (letter == ll) {
                    has_double = true;
                }

                if (ll == 'a' and letter == 'b' or
                    ll == 'c' and letter == 'd' or
                    ll == 'p' and letter == 'q' or
                    ll == 'x' and letter == 'y')
                {
                    has_illegal_strings = true;
                }
            }

            last_letter = letter;
        }

        if (vowel_count >= 3 and has_double and !has_illegal_strings) {
            nice_strings += 1;
        }
    }
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Part 1: {}\n", .{nice_strings});
}

fn part2() !void {
    var input_file = try std.fs.cwd().openFile("5input", .{});
    defer input_file.close();

    const reader = &input_file.reader();

    var nice_strings: usize = 0;
    while (true) {
        var buffer: [16]u8 = undefined;
        const read_bytes = try reader.read(&buffer);
        if (read_bytes < 16) {
            break;
        }
        try reader.skipBytes(1, .{});

        var found_pair = false;
        var i: usize = 0;
        pair_loop: while (i < 13) : (i += 1) {
            var j = i + 2;
            while (j < 15) : (j += 1) {
                if (buffer[i] == buffer[j] and buffer[i + 1] == buffer[j + 1]) {
                    found_pair = true;
                    break :pair_loop;
                }
            }
        }
        if (!found_pair) {
            continue;
        }

        var found_repeat = false;
        i = 0;
        while (i < 14) : (i += 1) {
            if (buffer[i] == buffer[i + 2]) {
                found_repeat = true;
                break;
            }
        }

        if (found_repeat) {
            nice_strings += 1;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Part 2: {}\n", .{nice_strings});
}
