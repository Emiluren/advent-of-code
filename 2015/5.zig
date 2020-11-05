const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var input_file = try std.fs.cwd().openFile("5input", .{});
    defer input_file.close();

    const in_stream = &input_file.inStream();

    const vowels = "aeiou";

    var nice_strings: usize = 0;
    while (true) {
        var buffer: [16]u8 = undefined;
        const read_bytes = try in_stream.read(&buffer);
        if (read_bytes < 16) {
            break;
        }
        try in_stream.skipBytes(1);

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

                if (
                    ll == 'a' and letter == 'b' or
                    ll == 'c' and letter == 'd' or
                    ll == 'p' and letter == 'q' or
                    ll == 'x' and letter == 'y'
                ) {
                    has_illegal_strings = true;
                }
            }

            last_letter = letter;
        }

        if (vowel_count >= 3 and has_double and !has_illegal_strings) {
            nice_strings += 1;
        }
    }
    const stdout = std.io.getStdOut().outStream();
    try stdout.print("Part 1: {}\n", .{ nice_strings });
}
