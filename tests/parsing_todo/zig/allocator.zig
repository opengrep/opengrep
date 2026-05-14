const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const buf = try allocator.alloc(u8, 64);
    defer allocator.free(buf);

    @memset(buf, 0);
    std.debug.print("buf len: {d}\n", .{buf.len});
}
