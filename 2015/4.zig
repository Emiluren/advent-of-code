const std = @import("std");

pub fn main() !void {
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
        while (i < 5) : (i += 1) {
            if (hashString[i] != '0') {
                continue :outer;
            }
        }

        break;
    }

    const stdout = std.io.getStdOut().outStream();
    try stdout.print("Part 1: {}\n", .{key});
}
