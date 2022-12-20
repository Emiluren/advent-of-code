const std = @import("std");

const V2 = @Vector(2, usize);

const SearchState = struct {
    pos: V2,
    steps: usize,
};

const Map = [44][66]u8;

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var file = try std.fs.cwd().openFile("12input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var elevation: Map = undefined;
    var start_pos: V2 = undefined;
    var end_pos: V2 = undefined;

    var row: usize = 0;
    while (row < elevation.len) : (row += 1) {
        var col: usize = 0;
        while (col < elevation[0].len) : (col += 1) {
            elevation[row][col] = 255;
        }
    }

    row = 1;
    var buf: [65]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| : (row += 1) {
        if (line.len == 0) break;

        for (line) |c, col| {
            var pos = V2{ row, col+1 };
            elevation[row][col+1] = switch (c) {
                'S' => blk: {
                    start_pos = pos;
                    break :blk 0;
                },
                'E' => blk: {
                    end_pos = pos;
                    break :blk 'z' - 'a';
                },
                else => c - 'a',
            };
        }
    }

    const part1 = (try distance(start_pos, end_pos, elevation)).?;
    var part2: usize = part1;
    for (elevation) |r, ri| {
        for (r) |e, ci| {
            if (e != 0)
                continue;
            part2 = std.math.min(part2, try distance(V2 { ri, ci }, end_pos, elevation) orelse part2);
        }
    }

    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}

fn distance(start_pos: V2, end_pos: V2, elevation: Map) !?usize {
    var row: usize = 0;
    var visited: [44][66]bool = undefined;
    while (row < elevation.len) : (row += 1) {
        var col: usize = 0;
        while (col < elevation[0].len) : (col += 1) {
            visited[row][col] = false;
        }
    }
    visited[start_pos[0]][start_pos[1]] = true;

    var queue = std.fifo.LinearFifo(
        SearchState, std.fifo.LinearFifoBufferType { .Static = 64 * 42 }
    ).init();
    try queue.writeItem(.{ .pos = start_pos, .steps = 0 });

    while (queue.readItem()) |state| {
        const r = state.pos[0];
        const c = state.pos[1];
        if (r == end_pos[0] and c == end_pos[1]) {
            return state.steps;
        }

        const candidates = [_]V2{
            V2 { r-1, c },
            V2 { r+1, c },
            V2 { r, c-1 },
            V2 { r, c+1 },
        };

        for (candidates) |new_pos| {
            if (visited[new_pos[0]][new_pos[1]])
                continue;

            const new_e = elevation[new_pos[0]][new_pos[1]];
            if (new_e <= elevation[r][c] + 1) {
                try queue.writeItem(.{ .pos = new_pos, .steps = state.steps + 1 });
                visited[new_pos[0]][new_pos[1]] = true;
            }
        }
    }
    return null;
}
