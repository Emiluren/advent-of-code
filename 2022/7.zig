const std = @import("std");

const File = struct {
    size: u32,
    name: []const u8,

    fn create(size: u32, name: []const u8, allocator: std.mem.Allocator) !*File {
        var file = try allocator.create(File);
        file.size = size;
        file.name = name;
        return file;
    }
};
const Dir = struct {
    children: [16]?Node,
    parent: ?*Dir,
    name: []const u8,
    size: u32,

    fn init(parent: ?*Dir, name: []const u8) Dir {
        var dir = Dir { .children = undefined, .parent = parent, .name = name, .size = 0 };
        for (dir.children) |*child| {
            child.* = null;
        }
        return dir;
    }

    fn create(parent: ?*Dir, name: []const u8, allocator: std.mem.Allocator) !*Dir {
        var dir = try allocator.create(Dir);
        dir.* = Dir.init(parent, name);
        return dir;
    }
};

const NodeTag = enum { file, dir };
const Node = union(NodeTag) { file: *File, dir: *Dir };

const input: [11141]u8 = @embedFile("7input").*;

pub fn main() !void {
    const start_time = std.time.microTimestamp();

    var root = Dir.init(null, "/");
    var current_dir = &root;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var line_iter = std.mem.tokenize(u8, &input, "\n");
    while (line_iter.next()) |line| {
        if (line[0] == '$') {
            if (line[2] == 'l') { // ls
                for (current_dir.children) |*child| {
                    const ls_line = line_iter.peek() orelse break;
                    if (ls_line[0] == '$')
                        break;

                    var tok_iter = std.mem.tokenize(u8, ls_line, " ");
                    const size_str = tok_iter.next().?;
                    const name = tok_iter.next().?;
                    if (ls_line[0] == 'd') { // dir
                        child.* = Node{ .dir = try Dir.create(current_dir, name, allocator) };
                    } else {
                        const size = try std.fmt.parseInt(u32, size_str, 10);
                        child.* = Node{ .file = try File.create(size, name, allocator) };
                    }

                    _ = line_iter.next();
                }
            } else {
                std.debug.assert(line[2] == 'c'); // cd
                const target = line[5..];
                if (target[0] == '/') {
                    current_dir = &root; // Should only happen at line 0 but whatever
                } else if (target[0] == '.') {
                    current_dir = current_dir.parent.?;
                } else for (current_dir.children) |child| {
                    switch (child.?) {
                        .dir => |dir| if (std.mem.eql(u8, target, dir.name)) {
                            current_dir = dir;
                            break;
                        },
                        else => {},
                    }
                }
            }
        }
    }

    _ = calc_size(Node{ .dir = &root });
    //print_tree(Node{ .dir = &root }, 0);

    var sizes = std.ArrayList(u32).init(allocator);
    try get_dir_sizes(Node{ .dir = &root }, &sizes);
    const needed = root.size - 40_000_000;

    var part1: u32 = 0;
    var part2: u32 = 30_000_000;
    for (sizes.items) |size| {
        if (size < 100_000)
            part1 += size;
        if (size < part2 and size >= needed)
            part2 = size;
    }

    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Part 1: {d}\nPart 2: {d}\nTime in µs: {d}\n", .{part1, part2, time});
}

fn calc_size(node: Node) u32 {
    switch (node) {
        .file => |f| return f.size,
        .dir => |d| {
            for (d.children) |child| {
                if (child) |c| {
                    d.size += calc_size(c);
                }
            }
            return d.size;
        },
    }
}

fn get_dir_sizes(node: Node, sizes: *std.ArrayList(u32)) !void {
    switch (node) {
        .dir => |d| {
            try sizes.append(d.size);
            for (d.children) |child| {
                if (child) |c| {
                    try get_dir_sizes(c, sizes);
                }
            }
        },
        else => {},
    }
}

fn print_tree(node: Node, level: usize) void {
    var i: usize = 0;
    while (i < level+1) : (i += 1) {
        std.debug.print("  ", .{});
    }
    switch (node) {
        .file => |f| std.debug.print("{d} {s}\n", .{f.size, f.name}),
        .dir => |d| {
            std.debug.print("{s}\n", .{d.name});
            for (d.children) |child| {
                if (child) |c| {
                    print_tree(c, level+1);
                }
            }
        },
    }
}
