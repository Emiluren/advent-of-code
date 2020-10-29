const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().outStream();
    try stdout.print("Part 1: {}\n", .{solve(5)});
    try stdout.print("Part 2: {}\n", .{solve(6)});
}

pub fn solve(limit: usize) !usize {
    const input = "bgvyzdsv";

    var concatBuffer: [100]u8 = undefined;
    var hashBuffer: [16]u8 = undefined;
    var hashStringBuffer: [32]u8 = undefined;

    var key: usize = 0;
    outer: while (true) : (key += 1) {
        const concatRes = try std.fmt.bufPrint(concatBuffer[0..], "{}{}", .{ input, key });
        std.crypto.Md5.hash(concatRes, hashBuffer[0..]);
        const hashString = try std.fmt.bufPrint(hashStringBuffer[0..], "{x}", .{hashBuffer});

        var i: usize = 0;
        while (i < limit) : (i += 1) {
            if (hashString[i] != '0') {
                continue :outer;
            }
        }

        return key;
    }
}
