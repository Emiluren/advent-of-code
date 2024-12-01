const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("1input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var buf: [16]u8 = undefined;
    var list1 = std.ArrayList(i32).init(allocator);
    var list2 = std.ArrayList(i32).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.splitSequence(u8, line, "   ");
        const int1 = try std.fmt.parseInt(i32, it.next().?, 10);
        const int2 = try std.fmt.parseInt(i32, it.next().?, 10);
        try list1.append(int1);
        try list2.append(int2);
    }

    std.mem.sort(i32, list1.items, {}, comptime std.sort.asc(i32));
    std.mem.sort(i32, list2.items, {}, comptime std.sort.asc(i32));
    var sum: u32 = 0;
    for (list1.items, list2.items) |int1, int2| {
        sum += @abs(int1 - int2);
    }
    std.debug.print("Part 1: {d}\n", .{sum});

    var sim_score: i32 = 0;
    for (list1.items) |int1| {
        var count: i32 = 0;
        for (list2.items) |int2| {
            if (int1 == int2) {
                count += 1;
            }
        }
        sim_score += int1 * count;
    }
    std.debug.print("Part 2: {d}\n", .{sim_score});
}
