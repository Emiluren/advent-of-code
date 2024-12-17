const std = @import("std");

const vec2 = @Vector(2, isize);
const Bot = struct { p: vec2, v: vec2 };

fn parsePair(str: []const u8) !vec2 {
    var it = std.mem.splitScalar(u8, str, ',');
    const x = it.next().?;
    const y = it.next().?;

    return .{
        try std.fmt.parseInt(isize, x, 10),
        try std.fmt.parseInt(isize, y, 10),
    };
}

fn calc_safety_factor(bots: [] const Bot) usize {
    var nw: usize = 0;
    var ne: usize = 0;
    var sw: usize = 0;
    var se: usize = 0;

    for (bots) |b| {
        const x = b.p[0];
        const y = b.p[1];

        if (x < 50) {
            if (y < 51) {
                nw += 1;
            } else if (y > 51) {
                sw += 1;
            }
        } else if (x > 50) {
            if (y < 51) {
                ne += 1;
            } else if (y > 51) {
                se += 1;
            }
        }
    }

    return nw * ne * sw * se;
}

fn print_bots(bots: [] const Bot) void {
    for (0..103) |y| {
        x_loop: for (0..101) |x| {
            for (bots) |b| {
                if (b.p[0] == x and b.p[1] == y) {
                    std.debug.print("O", .{});
                    continue :x_loop;
                }
            }
            std.debug.print(".", .{});
        }
        std.debug.print("\n", .{});
    }
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("14input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: std.BoundedArray(u8, 20) = .{};
    var bots: std.BoundedArray(Bot, 500) = .{};
    while (true) {
        in_stream.streamUntilDelimiter(buf.writer(), '\n', buf.capacity()) catch |err| switch (err) {
            error.EndOfStream => break, else => |e| return e,
        };
        defer buf.clear();

        var it = std.mem.splitSequence(u8, buf.slice(), " v=");
        const p = try parsePair(it.next().?[2..]);
        const v = try parsePair(it.next().?);

        try bots.append(.{.p = p, .v = v});
    }

    for (0..100) |_| {
        for (bots.slice()) |*b| {
            b.*.p += b.*.v;
            b.*.p[0] = try std.math.mod(isize, b.*.p[0], 101);
            b.*.p[1] = try std.math.mod(isize, b.*.p[1], 103);
        }
    }
    var safety_factor = calc_safety_factor(bots.slice());
    std.debug.print("Part 1: {d}\n", .{safety_factor});

    var min_i: usize = 100;
    var min_safety_factor = safety_factor;
    var min_bots = bots;
    for (0..10000) |i| {
        for (bots.slice()) |*b| {
            b.*.p += b.*.v;
            b.*.p[0] = try std.math.mod(isize, b.*.p[0], 101);
            b.*.p[1] = try std.math.mod(isize, b.*.p[1], 103);
        }

        safety_factor = calc_safety_factor(bots.slice());
        if (safety_factor < min_safety_factor) {
            min_i = 101 + i;
            min_safety_factor = safety_factor;
            min_bots = bots;
        }
    }
    print_bots(min_bots.slice());
    std.debug.print("Part 2: {}\n", .{min_i});
}
