const std = @import("std");

const input: [4096]u8 = @embedFile("6input").*;

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var part1: usize = 0;
    var part2: usize = 0;
    var q1: [4]u8 = input[0..4].*;
    var q2: [14]u8 = input[0..14].*;
    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        q1[i%4] = input[i];
        q2[i%14] = input[i];

        if (part1 == 0 and allDistinct(&q1)) {
            part1 = i + 1;
        }
        if (allDistinct(&q2)) {
            part2 = i + 1;
            break;
        }
    }
    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}

fn allDistinct(q: []u8) bool {
    var set = std.StaticBitSet(26).initEmpty();
    for (q) |c| {
        const i = c - 'a';
        if (set.isSet(i)) {
            return false;
        }
        set.set(i);
    }
    return true;
}
