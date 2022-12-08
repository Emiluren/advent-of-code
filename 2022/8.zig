const std = @import("std");

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var file = try std.fs.cwd().openFile("8input", .{});
    defer file.close();

    var input: [9900]u8 = undefined;
    const bytes_read = try file.readAll(&input);
    var line_iter = std.mem.tokenize(u8, input[0..bytes_read], "\n");
    var input_grid: [99][]const u8 = undefined;
    var r: usize = 0;
    while (line_iter.next()) |line| : (r += 1) {
        input_grid[r] = line;
    }

    var part1: usize = 0;
    var part2: usize = 0;
    const grid = input_grid[0..r];
    r = 0;
    while (r < grid.len) : (r += 1) {
        var c: usize = 0;
        while (c < grid[r].len) : (c += 1) {
            if (isVisible(r, c, grid)) {
                part1 += 1;
            }
            part2 = std.math.max(part2, score(r, c, grid));
        }
    }
    std.debug.print("Part 1: {d}\nPart 2: {d}\n", .{part1, part2});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}

fn isVisible(r: usize, c: usize, grid: [][]const u8) bool {
    if (r == 0 or r == grid.len - 1 or c == 0 or c == grid[0].len - 1)
        return true;
    const h = grid[r][c];

    var cc: usize = c - 1;
    while (true) : (cc -= 1) {
        if (grid[r][cc] >= h)
            break;
        if (cc == 0)
            return true;
    }
    cc = c + 1;
    while (true) : (cc += 1) {
        if (grid[r][cc] >= h)
            break;
        if (cc == grid[r].len - 1)
            return true;
    }

    var rr: usize = r - 1;
    while (true) : (rr -= 1) {
        if (grid[rr][c] >= h)
            break;
        if (rr == 0)
            return true;
    }
    rr = r + 1;
    while (true) : (rr += 1) {
        if (grid[rr][c] >= h)
            break;
        if (rr == grid.len - 1)
            return true;
    }
    return false;
}

fn score(r: usize, c: usize, grid: [][]const u8) usize {
    if (r == 0 or r == grid.len - 1 or c == 0 or c == grid[0].len - 1)
        return 0;
    const h = grid[r][c];
    var s: usize = 1;

    var cc: usize = c - 1;
    while (cc > 0) : (cc -= 1) {
        if (grid[r][cc] >= h)
            break;
    }
    s *= c - cc;
    cc = c + 1;
    while (cc < grid[r].len - 1) : (cc += 1) {
        if (grid[r][cc] >= h)
            break;
    }
    s *= cc - c;

    var rr: usize = r - 1;
    while (rr > 0) : (rr -= 1) {
        if (grid[rr][c] >= h)
            break;
    }
    s *= r - rr;
    rr = r + 1;
    while (rr < grid.len - 1) : (rr += 1) {
        if (grid[rr][c] >= h)
            break;
    }
    s *= rr - r;
    return s;
}
