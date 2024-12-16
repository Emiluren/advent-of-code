const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("14input", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: std.BoundedArray(u8, 131) = .{};
    while (true) {
        in_stream.streamUntilDelimiter(buf.writer(), '\n', buf.capacity()) catch |err| switch (err) {
            error.EndOfStream => {
                break;
            },
            else => |e| return e,
        };
        defer buf.clear();

        var it = std.mem.splitSequence(u8, buf.slice(), " v=");
        try p_str = it.next();
        try v_str = it.next();

        it = std.mem.splitScalar(u8, )
    }
}
