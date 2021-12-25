const std = @import("std");
const vm = @cImport({
    @cInclude("vm.h");
});

const PROGRAM_SIZE = 1024 * 1024;

pub fn main() anyerror!void {
    var program: [PROGRAM_SIZE]u8 = undefined;
    const buf = try std.fs.cwd().readFile("a.bin", &program);
    const result = vm.interpret(buf.ptr);

    std.os.exit(if (result == 0) 0 else 2);
}

test "basic test" {
    try std.testing.expectEqual(42, 1 + 41);
}
