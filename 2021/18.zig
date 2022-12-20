const std = @import("std");

const SnailfishTag = enum {pair, val};

const Snailfish = union(SnailfishTag) {
    pair: struct {sn1: *Snailfish, sn2: *Snailfish},
    val: u8,
};

pub fn main() !void {
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    // var allocator = &arena.allocator;

    var fish = Snailfish {
        .val = 5
    };

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{ fish });
}
