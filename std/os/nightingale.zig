const std = @import("../std.zig");
const assert = std.debug.assert;
const builtin = @import("builtin");
const maxInt = std.math.maxInt;
pub use switch (builtin.arch) {
    builtin.Arch.x86_64 => @import("nightingale/x86_64.zig"),
    else => @compileError("unsupported arch"),
};
pub use @import("nightingale/errno.zig");

pub const STDIN_FILENO = 0;
pub const STDOUT_FILENO = 1;
pub const STDERR_FILENO = 2;

pub const WNOHANG = 1;

pub const SEEK_SET = 0;
pub const SEEK_CUR = 1;
pub const SEEK_END = 2;

pub const PROT_READ = 1;
pub const PROT_WRITE = 2;

pub const MAP_SHARED = 1;
pub const MAP_ANONYMOUS = 2;
pub const MAP_PRIVATE = 3;
pub const MAP_FAILED = 4;

pub const PidT = i32;

pub fn getErrno(r: usize) usize {
    const signed_r = @bitCast(isize, r);
    if (signed_r < 0)  return @bitCast(usize, -signed_r);
    return 0;
}

pub fn exit(status: isize) noreturn {
    _ = syscall1(SYS_EXIT, @intCast(usize, status));
    unreachable;
}

pub fn exit_group(status: isize) noreturn {
    _ = syscall1(SYS_EXIT_GROUP, status);
    unreachable;
}

pub fn n_read(fd: isize, data: []u8) usize {
    return syscall3(SYS_READ, @intCast(usize, fd), @ptrToInt(data.ptr), data.len);
}

pub fn n_write(fd: isize, data: []const u8) usize {
    return syscall3(SYS_WRITE, @intCast(usize, fd), @ptrToInt(data.ptr), data.len);
}

pub fn read(fd: isize, data: [*]u8, len: usize) usize {
    return syscall3(SYS_READ, @intCast(usize, fd), @ptrToInt(data), len);
}

pub fn write(fd: isize, data: [*]const u8, len: usize) usize {
    return syscall3(SYS_WRITE, @intCast(usize, fd), @ptrToInt(data), len);
}

pub fn fork() !PidT {
    return syscall0(SYS_FORK);
}

pub fn getpid() !PidT {
    return syscall0(SYS_GETPID);
}

pub fn gettid() !PidT {
    return syscall0(SYS_GETTID);
}

pub fn execve(program: []const u8, argv: []const [*]u8) usize {
    return syscall3(SYS_EXECVE, @ptrToInt(program.ptr), @ptrToInt(argv.ptr));
}

pub fn strace(enable: bool) usize {
    return syscall1(SYS_STRACE, enable);
}

pub fn mmap(addr: ?*c_void, len: usize, prot: isize, flags: isize, fd: isize, off: isize) usize {
    return syscall6(SYS_MMAP, @ptrToInt(addr), len, @intCast(usize, prot), @intCast(usize, flags), @intCast(usize, fd), @intCast(usize, off));
}

pub fn munmap(addr: usize, len: usize) usize {
    return syscall2(SYS_MUNMAP, addr, len);
}

pub fn setpgid() usize {
    return syscall0(SYS_SETPGID);
}

pub fn isatty(handle: i32) bool {
    return false;
}
