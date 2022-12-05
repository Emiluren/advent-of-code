const std = @import("std");

const stack_size = 64;

pub fn main() !void {
    var file = try std.fs.cwd().openFile("5input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var state1: [9]std.BoundedArray(u8, stack_size) = undefined;
    for (state1) |*stack| {
        stack.* = try std.BoundedArray(u8, stack_size).init(0);
    }

    var level: u8 = 0;
    var buf: [50]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| : (level += 1) {
        if (line[1] == '1') break;

        for (state1) |*stack, col| {
            if (line[col*4] == '[') {
                try stack.append(line[col*4 + 1]);
            }
        }
    }
    for (state1) |*stack| {
        std.mem.reverse(u8, stack.slice());
    }
    _ = try in_stream.readUntilDelimiterOrEof(&buf, '\n'); // Skip empty line

    var state2 = state1;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var split_it = std.mem.split(u8, line, " ");
        _ = split_it.next(); // Skip move
        const amount = try std.fmt.parseInt(u8, split_it.next().?, 10);
        _ = split_it.next(); // Skip from
        const from = try std.fmt.parseInt(u8, split_it.next().?, 10) - 1;
        _ = split_it.next(); // Skip to
        const to = try std.fmt.parseInt(u8, split_it.next().?, 10) - 1;

        var to_move = state1[from].slice()[state1[from].len - amount..];
        std.mem.reverse(u8, to_move);
        try state1[to].appendSlice(to_move);
        try state1[from].resize(state1[from].len - amount);

        try state2[to].appendSlice(state2[from].slice()[state2[from].len - amount..]);
        try state2[from].resize(state2[from].len - amount);
    }

    var part1: [9]u8 = undefined;
    var part2: [9]u8 = undefined;
    for (state1) |_, col| {
        part1[col] = state1[col].popOrNull() orelse '_';
        part2[col] = state2[col].popOrNull() orelse '_';
    }
    std.debug.print("Part 1: {s}\nPart 2: {s}\n", .{part1, part2});
}

fn print_state(state: []std.BoundedArray(u8, stack_size)) void {
    var height: usize = 0;
    for (state) |stack| {
        height = std.math.max(height, stack.len);
    }

    var level: usize = 0;
    while (level < height) : (level += 1) {
        for (state) |stack| {
            const i = height - level - 1;
            if (i < stack.len) {
                std.debug.print("[{c}] ", .{stack.get(i)});
            } else {
                std.debug.print("    ", .{});
            }
        }
        std.debug.print("\n", .{});
    }
    for (state) |_, i| {
        std.debug.print(" {d}  ", .{i});
    }
    std.debug.print("\n\n", .{});
}
