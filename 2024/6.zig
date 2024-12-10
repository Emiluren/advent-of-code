const std = @import("std");

fn rot(v: @Vector(2, isize)) @Vector(2, isize) {
    return .{ v[1], -v[0] };
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("6input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: std.BoundedArray(u8, 131) = .{};
    var height: usize = 0;
    var width: usize = 0;
    var pos: @Vector(2, isize) = undefined;
    var obstacles: [130][130]bool = [_][130]bool{[_]bool{false} ** 130} ** 130;
    while (true) {
        in_stream.streamUntilDelimiter(buf.writer(), '\n', buf.capacity()) catch |err| switch (err) {
            error.EndOfStream => {
                break;
            },
            else => |e| return e,
        };
        defer buf.clear();

        std.debug.print("{s} {d} {d}\n", .{buf.slice(), height, width});
        width = buf.len;

        if (std.mem.indexOfScalar(u8, buf.slice(), '^')) |i| {
            pos = .{ @intCast(height), @intCast(i) };
        }

        for (buf.slice(), 0..) |c, i| {
            if (c == '#') {
                obstacles[height][i] = true;
            }
        }
        height += 1;
    }
    std.debug.print("{d} {d}\n", .{pos[0], pos[1]});

    var dir: @Vector(2, isize) = .{ -1,  0 };
    var visited: [130][130]bool = [_][130]bool{[_]bool{false} ** 130} ** 130;
    while (true) {
        visited[@intCast(pos[0])][@intCast(pos[1])] = true;

        const new_pos = pos + dir;
        if (new_pos[0] < 0 or new_pos[1] < 0 or new_pos[0] >= height or new_pos[1] >= width) {
            break;
        }

        if (obstacles[@intCast(new_pos[0])][@intCast(new_pos[1])]) {
            dir = rot(dir);
        } else {
            pos = new_pos;
        }
    }

    var visited_count: usize = 0;
    std.debug.print("\n", .{});
    for (0..height) |r| {
        for (0..width) |c| {
            if (visited[r][c]) {
                std.debug.print("X", .{});
                visited_count += 1;
            } else if (obstacles[r][c]) {
                std.debug.print("#", .{});
            } else {
                std.debug.print(".", .{});
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("Part 1: {d}\n", .{ visited_count });
}
