const std = @import("std");

const State = struct {
    cycle: i32,
    x: i32,
    part1: i32,

    const Self = @This();

    fn init() Self {
        return .{
            .cycle = 1,
            .x = 1,
            .part1 = 0,
        };
    }

    fn run_cycle(self: *Self) void {
        const render_x = @mod(self.cycle, 40);
        if (render_x >= self.x and render_x < self.x + 3) {
            std.debug.print("#", .{});
        } else {
            std.debug.print(".", .{});
        }
        if (render_x == 0) {
            std.debug.print("\n", .{});
        }

        const m = @mod(self.cycle - 20, 40);
        if (m == 0)
            self.part1 += self.cycle * self.x;
        self.cycle += 1;
    }
};

pub fn main() !void {
    const start_time = std.time.microTimestamp();
    var file = try std.fs.cwd().openFile("10input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var state = State.init();

    var buf: [16]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var split_it = std.mem.split(u8, line, " ");
        const opcode = split_it.next().?;
        const amount = try std.fmt.parseInt(i32, split_it.next() orelse "0", 10);

        switch (opcode[0]) {
            'n' => {
                state.run_cycle();
            },
            'a' => {
                state.run_cycle();
                state.run_cycle();
                state.x += amount;
            },
            else => std.debug.panic("Invalid opcode {s}", .{opcode}),
        }
    }
    std.debug.print("Part 1: {d}\n", .{state.part1});
    const time = std.time.microTimestamp() - start_time;
    std.debug.print("Time in µs: {d}\n", .{time});
}
