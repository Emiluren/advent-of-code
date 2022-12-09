const std = @import("std");

const Pos = @Vector(2, u32);

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var file = try std.fs.cwd().openFile("9input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const grid_size = 40_000;
    var head = Pos{ grid_size / 2, grid_size / 2, };
    var tail = Pos{ grid_size / 2, grid_size / 2, };
    var grid = std.AutoHashMap(Pos, void).init(allocator);

    try grid.put(tail, {});
    _ = head;

    var buf: [5]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        _ = line;
    }
    var part1 = grid.count();
    var part2: usize = 0;
    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}
