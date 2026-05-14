const std = @import("std");

fn mayFail(x: i32) !i32 {
    if (x < 0) return error.Negative;
    return x * 2;
}

pub fn main() void {
    const result = mayFail(5) catch unreachable;
    std.debug.print("result: {d}\n", .{result});
}
