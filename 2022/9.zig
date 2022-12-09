const std = @import("std");

const Pos = @Vector(2, i32);

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var file = try std.fs.cwd().openFile("9input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var head = Pos{ 0, 0, };
    var tail = head;
    var grid = std.AutoHashMap(Pos, void).init(allocator);

    try grid.put(tail, {});

    //printGrid(head, tail);
    var buf: [5]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var split_it = std.mem.split(u8, line, " ");
        const dir = split_it.next().?[0];
        const steps = try std.fmt.parseInt(u8, split_it.next().?, 10);

        const head_dir = switch (dir) {
            'R' => Pos{1, 0},
            'D' => Pos{0, 1},
            'L' => Pos{-1, 0},
            else => Pos{0, -1},
        };

        //std.debug.print("== {c} {d} ==\n\n", .{dir, steps});

        var i: u8 = 0;
        while (i < steps) : (i += 1) {
            head += head_dir;
            var delta = head - tail;
            if (try std.math.absInt(delta[0]) < 2 and try std.math.absInt(delta[1]) < 2) delta = Pos { 0, 0 };
            if (delta[0] < -1) delta[0] = -1 else if (delta[0] > 1) delta[0] = 1;
            if (delta[1] < -1) delta[1] = -1 else if (delta[1] > 1) delta[1] = 1;
            tail += delta;
            try grid.put(tail, {});
            //printGrid(head, tail);
        }
    }
    var part1 = grid.count();
    var part2: usize = 0;
    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}

fn printGrid(head: Pos, tail: Pos) void {
    var y: i32 = -4;

    while (y < 1) : (y += 1) {
        var x: i32 = 0;
        while (x < 6) : (x += 1) {
            const c: u8 = if (x == head[0] and y == head[1]) 'H'
                else if (x == tail[0] and y == tail[1]) 'T'
                else '.';
            std.debug.print("{c}", .{c});
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}
