const std = @import("std.zig");
const builtin = @import("builtin");
const Os = builtin.Os;
const is_windows = builtin.os == Os.windows;
const is_posix = switch (builtin.os) {
    builtin.Os.linux, builtin.Os.macosx, builtin.Os.freebsd, builtin.Os.netbsd, builtin.Os.nightingale => true,
    else => false,
};
const os = @This();

comptime {
    assert(@import("std") == std); // You have to run the std lib tests with --override-std-dir
}

test "std.os" {
    _ = @import("os/child_process.zig");
    _ = @import("os/darwin.zig");
    _ = @import("os/darwin/errno.zig");
    _ = @import("os/get_user_id.zig");
    _ = @import("os/linux.zig");
    _ = @import("os/path.zig");
    _ = @import("os/test.zig");
    _ = @import("os/time.zig");
    _ = @import("os/windows.zig");
    _ = @import("os/uefi.zig");
    _ = @import("os/wasi.zig");
    _ = @import("os/get_app_data_dir.zig");
}

pub const windows = @import("os/windows.zig");
pub const darwin = @import("os/darwin.zig");
pub const linux = @import("os/linux.zig");
pub const freebsd = @import("os/freebsd.zig");
pub const netbsd = @import("os/netbsd.zig");
pub const zen = @import("os/zen.zig");
pub const uefi = @import("os/uefi.zig");
pub const wasi = @import("os/wasi.zig");
pub const nightingale = @import("os/nightingale.zig");

pub const posix = switch (builtin.os) {
    Os.linux => linux,
    Os.macosx, Os.ios => darwin,
    Os.freebsd => freebsd,
    Os.netbsd => netbsd,
    Os.zen => zen,
    Os.wasi => wasi,
    Os.nightingale => nightingale,
    else => @compileError("Unsupported OS"),
};

pub const net = @import("net.zig");

pub const ChildProcess = @import("os/child_process.zig").ChildProcess;
pub const path = @import("os/path.zig");
pub const File = @import("os/file.zig").File;
pub const time = @import("os/time.zig");

pub const page_size = switch (builtin.arch) {
    .wasm32, .wasm64 => 64 * 1024,
    else => 4 * 1024,
};

pub const MAX_PATH_BYTES = switch (builtin.os) {
    Os.linux, Os.macosx, Os.ios, Os.freebsd, Os.netbsd => posix.PATH_MAX,
    // Each UTF-16LE character may be expanded to 3 UTF-8 bytes.
    // If it would require 4 UTF-8 bytes, then there would be a surrogate
    // pair in the UTF-16LE, and we (over)account 3 bytes for it that way.
    // +1 for the null byte at the end, which can be encoded in 1 byte.
    Os.windows => windows_util.PATH_MAX_WIDE * 3 + 1,
    else => @compileError("Unsupported OS"),
};

pub const UserInfo = @import("os/get_user_id.zig").UserInfo;
pub const getUserInfo = @import("os/get_user_id.zig").getUserInfo;

const windows_util = @import("os/windows/util.zig");
pub const windowsWaitSingle = windows_util.windowsWaitSingle;
pub const windowsWrite = windows_util.windowsWrite;
pub const windowsIsCygwinPty = windows_util.windowsIsCygwinPty;
pub const windowsOpen = windows_util.windowsOpen;
pub const windowsOpenW = windows_util.windowsOpenW;
pub const createWindowsEnvBlock = windows_util.createWindowsEnvBlock;

pub const WindowsCreateIoCompletionPortError = windows_util.WindowsCreateIoCompletionPortError;
pub const windowsCreateIoCompletionPort = windows_util.windowsCreateIoCompletionPort;

pub const WindowsPostQueuedCompletionStatusError = windows_util.WindowsPostQueuedCompletionStatusError;
pub const windowsPostQueuedCompletionStatus = windows_util.windowsPostQueuedCompletionStatus;

pub const WindowsWaitResult = windows_util.WindowsWaitResult;
pub const windowsGetQueuedCompletionStatus = windows_util.windowsGetQueuedCompletionStatus;

pub const WindowsWaitError = windows_util.WaitError;
pub const WindowsOpenError = windows_util.OpenError;
pub const WindowsWriteError = windows_util.WriteError;
pub const WindowsReadError = windows_util.ReadError;

pub const FileHandle = if (is_windows) windows.HANDLE else i32;

pub const getAppDataDir = @import("os/get_app_data_dir.zig").getAppDataDir;
pub const GetAppDataDirError = @import("os/get_app_data_dir.zig").GetAppDataDirError;

const debug = std.debug;
const assert = debug.assert;
const testing = std.testing;

const c = std.c;

const mem = std.mem;
const Allocator = mem.Allocator;

const BufMap = std.BufMap;
const cstr = std.cstr;

const io = std.io;
const base64 = std.base64;
const ArrayList = std.ArrayList;
const Buffer = std.Buffer;
const math = std.math;

/// Fills `buf` with random bytes. If linking against libc, this calls the
/// appropriate OS-specific library call. Otherwise it uses the zig standard
/// library implementation.
pub fn getRandomBytes(buf: []u8) !void {
    switch (builtin.os) {
        Os.linux => while (true) {
            // TODO check libc version and potentially call c.getrandom.
            // See #397
            const errno = posix.getErrno(posix.getrandom(buf.ptr, buf.len, 0));
            switch (errno) {
                0 => return,
                posix.EINVAL => unreachable,
                posix.EFAULT => unreachable,
                posix.EINTR => continue,
                posix.ENOSYS => return getRandomBytesDevURandom(buf),
                else => return unexpectedErrorPosix(errno),
            }
        },
        Os.macosx, Os.ios, Os.freebsd, Os.netbsd => return getRandomBytesDevURandom(buf),
        Os.windows => {
            // Call RtlGenRandom() instead of CryptGetRandom() on Windows
            // https://github.com/rust-lang-nursery/rand/issues/111
            // https://bugzilla.mozilla.org/show_bug.cgi?id=504270
            if (windows.RtlGenRandom(buf.ptr, buf.len) == 0) {
                const err = windows.GetLastError();
                return switch (err) {
                    else => unexpectedErrorWindows(err),
                };
            }
        },
        Os.zen => {
            const randomness = []u8{ 42, 1, 7, 12, 22, 17, 99, 16, 26, 87, 41, 45 };
            var i: usize = 0;
            while (i < buf.len) : (i += 1) {
                if (i > randomness.len) return error.Unknown;
                buf[i] = randomness[i];
            }
        },
        else => @compileError("Unsupported OS"),
    }
}

fn getRandomBytesDevURandom(buf: []u8) !void {
    const fd = try posixOpenC(c"/dev/urandom", posix.O_RDONLY | posix.O_CLOEXEC, 0);
    defer close(fd);

    const stream = &File.openHandle(fd).inStream().stream;
    stream.readNoEof(buf) catch |err| switch (err) {
        error.EndOfStream => unreachable,
        error.OperationAborted => unreachable,
        error.BrokenPipe => unreachable,
        error.Unexpected => return error.Unexpected,
        error.InputOutput => return error.Unexpected,
        error.SystemResources => return error.Unexpected,
        error.IsDir => unreachable,
    };
}

test "os.getRandomBytes" {
    var buf_a: [50]u8 = undefined;
    var buf_b: [50]u8 = undefined;
    // Call Twice
    try getRandomBytes(buf_a[0..]);
    try getRandomBytes(buf_b[0..]);

    // Check if random (not 100% conclusive)
    testing.expect(!mem.eql(u8, buf_a, buf_b));
}

/// Raises a signal in the current kernel thread, ending its execution.
/// If linking against libc, this calls the abort() libc function. Otherwise
/// it uses the zig standard library implementation.
pub fn abort() noreturn {
    @setCold(true);
    if (builtin.link_libc) {
        c.abort();
    }
    switch (builtin.os) {
        Os.linux, Os.macosx, Os.ios, Os.freebsd, Os.netbsd, Os.wasi => {
            _ = posix.raise(posix.SIGABRT);
            _ = posix.raise(posix.SIGKILL);
            while (true) {}
        },
        Os.windows => {
            if (builtin.mode == builtin.Mode.Debug) {
                @breakpoint();
            }
            windows.ExitProcess(3);
        },
        Os.uefi => {
            // TODO there's gotta be a better thing to do here than loop forever
            while (true) {}
        },
        Os.nightingale => {
            while(true) {}
        },
        else => @compileError("Unsupported OS"),
    }
}

/// Exits the program cleanly with the specified status code.
pub fn exit(status: u8) noreturn {
    @setCold(true);
    if (builtin.link_libc) {
        c.exit(status);
    }
    switch (builtin.os) {
        Os.linux => {
            if (builtin.single_threaded) {
                linux.exit(status);
            } else {
                linux.exit_group(status);
            }
        },
        Os.macosx, Os.ios, Os.freebsd, Os.netbsd => {
            posix.exit(status);
        },
        Os.windows => {
            windows.ExitProcess(status);
        },
        Os.nightingale => {
            if (builtin.single_threaded) {
                try nightingale.exit(status);
            } else {
                try nightingale.exit_group(status);
            }
        },
        else => @compileError("Unsupported OS"),
    }
}

/// When a file descriptor is closed on linux, it pops the first
/// node from this queue and resumes it.
/// Async functions which get the EMFILE error code can suspend,
/// putting their coroutine handle into this list.
/// TODO make this an atomic linked list
pub var emfile_promise_queue = std.LinkedList(promise).init();

/// Closes the file handle. Keeps trying if it gets interrupted by a signal.
pub fn close(handle: FileHandle) void {
    if (is_windows) {
        windows_util.windowsClose(handle);
    } else {
        while (true) {
            const err = posix.getErrno(posix.close(handle));
            switch (err) {
                posix.EINTR => continue,
                else => {
                    if (emfile_promise_queue.popFirst()) |p| resume p.data;
                    return;
                },
            }
        }
    }
}

pub const PosixReadError = error{
    InputOutput,
    SystemResources,
    IsDir,
    Unexpected,
};

/// Returns the number of bytes that were read, which can be less than
/// buf.len. If 0 bytes were read, that means EOF.
pub fn posixRead(fd: i32, buf: []u8) PosixReadError!usize {
    // Linux can return EINVAL when read amount is > 0x7ffff000
    // See https://github.com/ziglang/zig/pull/743#issuecomment-363158274
    const max_buf_len = 0x7ffff000;

    var index: usize = 0;
    while (index < buf.len) {
        const want_to_read = math.min(buf.len - index, usize(max_buf_len));
        const rc = posix.read(fd, buf.ptr + index, want_to_read);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => {
                index += rc;
                if (rc == want_to_read) continue;
                // Read returned less than buf.len.
                return index;
            },
            posix.EINTR => continue,
            posix.EINVAL => unreachable,
            posix.EFAULT => unreachable,
            posix.EAGAIN => unreachable,
            posix.EBADF => unreachable, // always a race condition
            posix.EIO => return error.InputOutput,
            posix.EISDIR => return error.IsDir,
            posix.ENOBUFS => return error.SystemResources,
            posix.ENOMEM => return error.SystemResources,
            else => return unexpectedErrorPosix(err),
        }
    }
    return index;
}

/// Number of bytes read is returned. Upon reading end-of-file, zero is returned.
pub fn posix_preadv(fd: i32, iov: [*]const posix.iovec, count: usize, offset: u64) PosixReadError!usize {
    switch (builtin.os) {
        builtin.Os.macosx => {
            // Darwin does not have preadv but it does have pread.
            var off: usize = 0;
            var iov_i: usize = 0;
            var inner_off: usize = 0;
            while (true) {
                const v = iov[iov_i];
                const rc = darwin.pread(fd, v.iov_base + inner_off, v.iov_len - inner_off, offset + off);
                const err = darwin.getErrno(rc);
                switch (err) {
                    0 => {
                        off += rc;
                        inner_off += rc;
                        if (inner_off == v.iov_len) {
                            iov_i += 1;
                            inner_off = 0;
                            if (iov_i == count) {
                                return off;
                            }
                        }
                        if (rc == 0) return off; // EOF
                        continue;
                    },
                    posix.EINTR => continue,
                    posix.EINVAL => unreachable,
                    posix.EFAULT => unreachable,
                    posix.ESPIPE => unreachable, // fd is not seekable
                    posix.EAGAIN => unreachable, // this function is not for non blocking
                    posix.EBADF => unreachable, // always a race condition
                    posix.EIO => return error.InputOutput,
                    posix.EISDIR => return error.IsDir,
                    posix.ENOBUFS => return error.SystemResources,
                    posix.ENOMEM => return error.SystemResources,
                    else => return unexpectedErrorPosix(err),
                }
            }
        },
        builtin.Os.linux, builtin.Os.freebsd, Os.netbsd => while (true) {
            const rc = posix.preadv(fd, iov, count, offset);
            const err = posix.getErrno(rc);
            switch (err) {
                0 => return rc,
                posix.EINTR => continue,
                posix.EINVAL => unreachable,
                posix.EFAULT => unreachable,
                posix.EAGAIN => unreachable, // don't call this function for non blocking
                posix.EBADF => unreachable, // always a race condition
                posix.EIO => return error.InputOutput,
                posix.EISDIR => return error.IsDir,
                posix.ENOBUFS => return error.SystemResources,
                posix.ENOMEM => return error.SystemResources,
                else => return unexpectedErrorPosix(err),
            }
        },
        else => @compileError("Unsupported OS"),
    }
}

pub const PosixWriteError = error{
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    AccessDenied,
    BrokenPipe,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

/// Calls POSIX write, and keeps trying if it gets interrupted.
pub fn posixWrite(fd: i32, bytes: []const u8) PosixWriteError!void {
    // Linux can return EINVAL when write amount is > 0x7ffff000
    // See https://github.com/ziglang/zig/pull/743#issuecomment-363165856
    const max_bytes_len = 0x7ffff000;

    var index: usize = 0;
    while (index < bytes.len) {
        const amt_to_write = math.min(bytes.len - index, usize(max_bytes_len));
        const rc = posix.write(fd, bytes.ptr + index, amt_to_write);
        const write_err = posix.getErrno(rc);
        switch (write_err) {
            0 => {
                index += rc;
                continue;
            },
            posix.EINTR => continue,
            posix.EINVAL => unreachable,
            posix.EFAULT => unreachable,
            posix.EAGAIN => unreachable, // use posixAsyncWrite for non-blocking
            posix.EBADF => unreachable, // always a race condition
            posix.EDESTADDRREQ => unreachable, // connect was never called
            posix.EDQUOT => return PosixWriteError.DiskQuota,
            posix.EFBIG => return PosixWriteError.FileTooBig,
            posix.EIO => return PosixWriteError.InputOutput,
            posix.ENOSPC => return PosixWriteError.NoSpaceLeft,
            posix.EPERM => return PosixWriteError.AccessDenied,
            posix.EPIPE => return PosixWriteError.BrokenPipe,
            else => return unexpectedErrorPosix(write_err),
        }
    }
}

pub fn posix_pwritev(fd: i32, iov: [*]const posix.iovec_const, count: usize, offset: u64) PosixWriteError!void {
    switch (builtin.os) {
        builtin.Os.macosx => {
            // Darwin does not have pwritev but it does have pwrite.
            var off: usize = 0;
            var iov_i: usize = 0;
            var inner_off: usize = 0;
            while (true) {
                const v = iov[iov_i];
                const rc = darwin.pwrite(fd, v.iov_base + inner_off, v.iov_len - inner_off, offset + off);
                const err = darwin.getErrno(rc);
                switch (err) {
                    0 => {
                        off += rc;
                        inner_off += rc;
                        if (inner_off == v.iov_len) {
                            iov_i += 1;
                            inner_off = 0;
                            if (iov_i == count) {
                                return;
                            }
                        }
                        continue;
                    },
                    posix.EINTR => continue,
                    posix.ESPIPE => unreachable, // fd is not seekable
                    posix.EINVAL => unreachable,
                    posix.EFAULT => unreachable,
                    posix.EAGAIN => unreachable, // use posixAsyncPWriteV for non-blocking
                    posix.EBADF => unreachable, // always a race condition
                    posix.EDESTADDRREQ => unreachable, // connect was never called
                    posix.EDQUOT => return PosixWriteError.DiskQuota,
                    posix.EFBIG => return PosixWriteError.FileTooBig,
                    posix.EIO => return PosixWriteError.InputOutput,
                    posix.ENOSPC => return PosixWriteError.NoSpaceLeft,
                    posix.EPERM => return PosixWriteError.AccessDenied,
                    posix.EPIPE => return PosixWriteError.BrokenPipe,
                    else => return unexpectedErrorPosix(err),
                }
            }
        },
        builtin.Os.linux, builtin.Os.freebsd, builtin.Os.netbsd => while (true) {
            const rc = posix.pwritev(fd, iov, count, offset);
            const err = posix.getErrno(rc);
            switch (err) {
                0 => return,
                posix.EINTR => continue,
                posix.EINVAL => unreachable,
                posix.EFAULT => unreachable,
                posix.EAGAIN => unreachable, // use posixAsyncPWriteV for non-blocking
                posix.EBADF => unreachable, // always a race condition
                posix.EDESTADDRREQ => unreachable, // connect was never called
                posix.EDQUOT => return PosixWriteError.DiskQuota,
                posix.EFBIG => return PosixWriteError.FileTooBig,
                posix.EIO => return PosixWriteError.InputOutput,
                posix.ENOSPC => return PosixWriteError.NoSpaceLeft,
                posix.EPERM => return PosixWriteError.AccessDenied,
                posix.EPIPE => return PosixWriteError.BrokenPipe,
                else => return unexpectedErrorPosix(err),
            }
        },
        else => @compileError("Unsupported OS"),
    }
}

pub const PosixOpenError = error{
    AccessDenied,
    FileTooBig,
    IsDir,
    SymLinkLoop,
    ProcessFdQuotaExceeded,
    NameTooLong,
    SystemFdQuotaExceeded,
    NoDevice,
    FileNotFound,
    SystemResources,
    NoSpaceLeft,
    NotDir,
    PathAlreadyExists,
    DeviceBusy,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

/// ::file_path needs to be copied in memory to add a null terminating byte.
/// Calls POSIX open, keeps trying if it gets interrupted, and translates
/// the return value into zig errors.
pub fn posixOpen(file_path: []const u8, flags: u32, perm: usize) PosixOpenError!i32 {
    const file_path_c = try toPosixPath(file_path);
    return posixOpenC(&file_path_c, flags, perm);
}

// TODO https://github.com/ziglang/zig/issues/265
pub fn posixOpenC(file_path: [*]const u8, flags: u32, perm: usize) !i32 {
    while (true) {
        const result = posix.open(file_path, flags, perm);
        const err = posix.getErrno(result);
        if (err > 0) {
            switch (err) {
                posix.EINTR => continue,

                posix.EFAULT => unreachable,
                posix.EINVAL => unreachable,
                posix.EACCES => return PosixOpenError.AccessDenied,
                posix.EFBIG, posix.EOVERFLOW => return PosixOpenError.FileTooBig,
                posix.EISDIR => return PosixOpenError.IsDir,
                posix.ELOOP => return PosixOpenError.SymLinkLoop,
                posix.EMFILE => return PosixOpenError.ProcessFdQuotaExceeded,
                posix.ENAMETOOLONG => return PosixOpenError.NameTooLong,
                posix.ENFILE => return PosixOpenError.SystemFdQuotaExceeded,
                posix.ENODEV => return PosixOpenError.NoDevice,
                posix.ENOENT => return PosixOpenError.FileNotFound,
                posix.ENOMEM => return PosixOpenError.SystemResources,
                posix.ENOSPC => return PosixOpenError.NoSpaceLeft,
                posix.ENOTDIR => return PosixOpenError.NotDir,
                posix.EPERM => return PosixOpenError.AccessDenied,
                posix.EEXIST => return PosixOpenError.PathAlreadyExists,
                posix.EBUSY => return PosixOpenError.DeviceBusy,
                else => return unexpectedErrorPosix(err),
            }
        }
        return @intCast(i32, result);
    }
}

/// Used to convert a slice to a null terminated slice on the stack.
/// TODO well defined copy elision
pub fn toPosixPath(file_path: []const u8) ![posix.PATH_MAX]u8 {
    var path_with_null: [posix.PATH_MAX]u8 = undefined;
    if (file_path.len >= posix.PATH_MAX) return error.NameTooLong;
    mem.copy(u8, path_with_null[0..], file_path);
    path_with_null[file_path.len] = 0;
    return path_with_null;
}

pub fn posixDup2(old_fd: i32, new_fd: i32) !void {
    while (true) {
        const err = posix.getErrno(posix.dup2(old_fd, new_fd));
        if (err > 0) {
            return switch (err) {
                posix.EBUSY, posix.EINTR => continue,
                posix.EMFILE => error.ProcessFdQuotaExceeded,
                posix.EINVAL => unreachable,
                else => unexpectedErrorPosix(err),
            };
        }
        return;
    }
}

pub fn createNullDelimitedEnvMap(allocator: *Allocator, env_map: *const BufMap) ![]?[*]u8 {
    const envp_count = env_map.count();
    const envp_buf = try allocator.alloc(?[*]u8, envp_count + 1);
    mem.set(?[*]u8, envp_buf, null);
    errdefer freeNullDelimitedEnvMap(allocator, envp_buf);
    {
        var it = env_map.iterator();
        var i: usize = 0;
        while (it.next()) |pair| : (i += 1) {
            const env_buf = try allocator.alloc(u8, pair.key.len + pair.value.len + 2);
            @memcpy(env_buf.ptr, pair.key.ptr, pair.key.len);
            env_buf[pair.key.len] = '=';
            @memcpy(env_buf.ptr + pair.key.len + 1, pair.value.ptr, pair.value.len);
            env_buf[env_buf.len - 1] = 0;

            envp_buf[i] = env_buf.ptr;
        }
        assert(i == envp_count);
    }
    assert(envp_buf[envp_count] == null);
    return envp_buf;
}

pub fn freeNullDelimitedEnvMap(allocator: *Allocator, envp_buf: []?[*]u8) void {
    for (envp_buf) |env| {
        const env_buf = if (env) |ptr| ptr[0 .. cstr.len(ptr) + 1] else break;
        allocator.free(env_buf);
    }
    allocator.free(envp_buf);
}

/// This function must allocate memory to add a null terminating bytes on path and each arg.
/// It must also convert to KEY=VALUE\0 format for environment variables, and include null
/// pointers after the args and after the environment variables.
/// `argv[0]` is the executable path.
/// This function also uses the PATH environment variable to get the full path to the executable.
pub fn posixExecve(argv: []const []const u8, env_map: *const BufMap, allocator: *Allocator) !void {
    const argv_buf = try allocator.alloc(?[*]u8, argv.len + 1);
    mem.set(?[*]u8, argv_buf, null);
    defer {
        for (argv_buf) |arg| {
            const arg_buf = if (arg) |ptr| cstr.toSlice(ptr) else break;
            allocator.free(arg_buf);
        }
        allocator.free(argv_buf);
    }
    for (argv) |arg, i| {
        const arg_buf = try allocator.alloc(u8, arg.len + 1);
        @memcpy(arg_buf.ptr, arg.ptr, arg.len);
        arg_buf[arg.len] = 0;

        argv_buf[i] = arg_buf.ptr;
    }
    argv_buf[argv.len] = null;

    const envp_buf = try createNullDelimitedEnvMap(allocator, env_map);
    defer freeNullDelimitedEnvMap(allocator, envp_buf);

    const exe_path = argv[0];
    if (mem.indexOfScalar(u8, exe_path, '/') != null) {
        return posixExecveErrnoToErr(posix.getErrno(posix.execve(argv_buf[0].?, argv_buf.ptr, envp_buf.ptr)));
    }

    const PATH = getEnvPosix("PATH") orelse "/usr/local/bin:/bin/:/usr/bin";
    // PATH.len because it is >= the largest search_path
    // +1 for the / to join the search path and exe_path
    // +1 for the null terminating byte
    const path_buf = try allocator.alloc(u8, PATH.len + exe_path.len + 2);
    defer allocator.free(path_buf);
    var it = mem.tokenize(PATH, ":");
    var seen_eacces = false;
    var err: usize = undefined;
    while (it.next()) |search_path| {
        mem.copy(u8, path_buf, search_path);
        path_buf[search_path.len] = '/';
        mem.copy(u8, path_buf[search_path.len + 1 ..], exe_path);
        path_buf[search_path.len + exe_path.len + 1] = 0;
        err = posix.getErrno(posix.execve(path_buf.ptr, argv_buf.ptr, envp_buf.ptr));
        assert(err > 0);
        if (err == posix.EACCES) {
            seen_eacces = true;
        } else if (err != posix.ENOENT) {
            return posixExecveErrnoToErr(err);
        }
    }
    if (seen_eacces) {
        err = posix.EACCES;
    }
    return posixExecveErrnoToErr(err);
}

pub const PosixExecveError = error{
    SystemResources,
    AccessDenied,
    InvalidExe,
    FileSystem,
    IsDir,
    FileNotFound,
    NotDir,
    FileBusy,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

fn posixExecveErrnoToErr(err: usize) PosixExecveError {
    assert(err > 0);
    switch (err) {
        posix.EFAULT => unreachable,
        posix.E2BIG => return error.SystemResources,
        posix.EMFILE => return error.SystemResources,
        posix.ENAMETOOLONG => return error.SystemResources,
        posix.ENFILE => return error.SystemResources,
        posix.ENOMEM => return error.SystemResources,
        posix.EACCES => return error.AccessDenied,
        posix.EPERM => return error.AccessDenied,
        posix.EINVAL => return error.InvalidExe,
        posix.ENOEXEC => return error.InvalidExe,
        posix.EIO => return error.FileSystem,
        posix.ELOOP => return error.FileSystem,
        posix.EISDIR => return error.IsDir,
        posix.ENOENT => return error.FileNotFound,
        posix.ENOTDIR => return error.NotDir,
        posix.ETXTBSY => return error.FileBusy,
        else => return unexpectedErrorPosix(err),
    }
}

pub var linux_elf_aux_maybe: ?[*]std.elf.Auxv = null;
pub var posix_environ_raw: [][*]u8 = undefined;

/// See std.elf for the constants.
pub fn linuxGetAuxVal(index: usize) usize {
    if (builtin.link_libc) {
        return usize(std.c.getauxval(index));
    } else if (linux_elf_aux_maybe) |auxv| {
        var i: usize = 0;
        while (auxv[i].a_type != std.elf.AT_NULL) : (i += 1) {
            if (auxv[i].a_type == index)
                return auxv[i].a_un.a_val;
        }
    }
    return 0;
}

pub fn getBaseAddress() usize {
    switch (builtin.os) {
        builtin.Os.linux => {
            const base = linuxGetAuxVal(std.elf.AT_BASE);
            if (base != 0) {
                return base;
            }
            const phdr = linuxGetAuxVal(std.elf.AT_PHDR);
            return phdr - @sizeOf(std.elf.Ehdr);
        },
        builtin.Os.macosx, builtin.Os.freebsd, builtin.Os.netbsd => {
            return @ptrToInt(&std.c._mh_execute_header);
        },
        builtin.Os.windows => return @ptrToInt(windows.GetModuleHandleW(null)),
        else => @compileError("Unsupported OS"),
    }
}

/// Caller must free result when done.
/// TODO make this go through libc when we have it
pub fn getEnvMap(allocator: *Allocator) !BufMap {
    var result = BufMap.init(allocator);
    errdefer result.deinit();

    if (is_windows) {
        const ptr = windows.GetEnvironmentStringsW() orelse return error.OutOfMemory;
        defer assert(windows.FreeEnvironmentStringsW(ptr) != 0);

        var i: usize = 0;
        while (true) {
            if (ptr[i] == 0) return result;

            const key_start = i;

            while (ptr[i] != 0 and ptr[i] != '=') : (i += 1) {}
            const key_w = ptr[key_start..i];
            const key = try std.unicode.utf16leToUtf8Alloc(allocator, key_w);
            errdefer allocator.free(key);

            if (ptr[i] == '=') i += 1;

            const value_start = i;
            while (ptr[i] != 0) : (i += 1) {}
            const value_w = ptr[value_start..i];
            const value = try std.unicode.utf16leToUtf8Alloc(allocator, value_w);
            errdefer allocator.free(value);

            i += 1; // skip over null byte

            try result.setMove(key, value);
        }
    } else {
        for (posix_environ_raw) |ptr| {
            var line_i: usize = 0;
            while (ptr[line_i] != 0 and ptr[line_i] != '=') : (line_i += 1) {}
            const key = ptr[0..line_i];

            var end_i: usize = line_i;
            while (ptr[end_i] != 0) : (end_i += 1) {}
            const value = ptr[line_i + 1 .. end_i];

            try result.set(key, value);
        }
        return result;
    }
}

test "os.getEnvMap" {
    var env = try getEnvMap(std.debug.global_allocator);
    defer env.deinit();
}

/// TODO make this go through libc when we have it
pub fn getEnvPosix(key: []const u8) ?[]const u8 {
    for (posix_environ_raw) |ptr| {
        var line_i: usize = 0;
        while (ptr[line_i] != 0 and ptr[line_i] != '=') : (line_i += 1) {}
        const this_key = ptr[0..line_i];
        if (!mem.eql(u8, key, this_key)) continue;

        var end_i: usize = line_i;
        while (ptr[end_i] != 0) : (end_i += 1) {}
        const this_value = ptr[line_i + 1 .. end_i];

        return this_value;
    }
    return null;
}

pub const GetEnvVarOwnedError = error{
    OutOfMemory,
    EnvironmentVariableNotFound,

    /// See https://github.com/ziglang/zig/issues/1774
    InvalidUtf8,
};

/// Caller must free returned memory.
/// TODO make this go through libc when we have it
pub fn getEnvVarOwned(allocator: *mem.Allocator, key: []const u8) GetEnvVarOwnedError![]u8 {
    if (is_windows) {
        const key_with_null = try std.unicode.utf8ToUtf16LeWithNull(allocator, key);
        defer allocator.free(key_with_null);

        var buf = try allocator.alloc(u16, 256);
        defer allocator.free(buf);

        while (true) {
            const windows_buf_len = math.cast(windows.DWORD, buf.len) catch return error.OutOfMemory;
            const result = windows.GetEnvironmentVariableW(key_with_null.ptr, buf.ptr, windows_buf_len);

            if (result == 0) {
                const err = windows.GetLastError();
                return switch (err) {
                    windows.ERROR.ENVVAR_NOT_FOUND => error.EnvironmentVariableNotFound,
                    else => {
                        unexpectedErrorWindows(err) catch {};
                        return error.EnvironmentVariableNotFound;
                    },
                };
            }

            if (result > buf.len) {
                buf = try allocator.realloc(buf, result);
                continue;
            }

            return std.unicode.utf16leToUtf8Alloc(allocator, buf) catch |err| switch (err) {
                error.DanglingSurrogateHalf => return error.InvalidUtf8,
                error.ExpectedSecondSurrogateHalf => return error.InvalidUtf8,
                error.UnexpectedSecondSurrogateHalf => return error.InvalidUtf8,
                error.OutOfMemory => return error.OutOfMemory,
            };
        }
    } else {
        const result = getEnvPosix(key) orelse return error.EnvironmentVariableNotFound;
        return mem.dupe(allocator, u8, result);
    }
}

test "os.getEnvVarOwned" {
    var ga = debug.global_allocator;
    testing.expectError(error.EnvironmentVariableNotFound, getEnvVarOwned(ga, "BADENV"));
}

/// Caller must free the returned memory.
pub fn getCwdAlloc(allocator: *Allocator) ![]u8 {
    var buf: [MAX_PATH_BYTES]u8 = undefined;
    return mem.dupe(allocator, u8, try getCwd(&buf));
}

pub const GetCwdError = error{Unexpected};

/// The result is a slice of out_buffer.
pub fn getCwd(out_buffer: *[MAX_PATH_BYTES]u8) GetCwdError![]u8 {
    switch (builtin.os) {
        Os.windows => {
            var utf16le_buf: [windows_util.PATH_MAX_WIDE]u16 = undefined;
            const casted_len = @intCast(windows.DWORD, utf16le_buf.len); // TODO shouldn't need this cast
            const casted_ptr = ([*]u16)(&utf16le_buf); // TODO shouldn't need this cast
            const result = windows.GetCurrentDirectoryW(casted_len, casted_ptr);
            if (result == 0) {
                const err = windows.GetLastError();
                switch (err) {
                    else => return unexpectedErrorWindows(err),
                }
            }
            assert(result <= utf16le_buf.len);
            const utf16le_slice = utf16le_buf[0..result];
            // Trust that Windows gives us valid UTF-16LE.
            const end_index = std.unicode.utf16leToUtf8(out_buffer, utf16le_slice) catch unreachable;
            return out_buffer[0..end_index];
        },
        else => {
            const err = posix.getErrno(posix.getcwd(out_buffer, out_buffer.len));
            switch (err) {
                0 => return cstr.toSlice(out_buffer),
                posix.ERANGE => unreachable,
                else => return unexpectedErrorPosix(err),
            }
        },
    }
}

test "os.getCwd" {
    // at least call it so it gets compiled
    _ = getCwdAlloc(debug.global_allocator) catch undefined;
    var buf: [MAX_PATH_BYTES]u8 = undefined;
    _ = getCwd(&buf) catch undefined;
}

pub const SymLinkError = PosixSymLinkError || WindowsSymLinkError;

/// TODO add a symLinkC variant
pub fn symLink(existing_path: []const u8, new_path: []const u8) SymLinkError!void {
    if (is_windows) {
        return symLinkWindows(existing_path, new_path);
    } else {
        return symLinkPosix(existing_path, new_path);
    }
}

pub const WindowsSymLinkError = error{
    NameTooLong,
    InvalidUtf8,
    BadPathName,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn symLinkW(existing_path_w: [*]const u16, new_path_w: [*]const u16) WindowsSymLinkError!void {
    if (windows.CreateSymbolicLinkW(existing_path_w, new_path_w, 0) == 0) {
        const err = windows.GetLastError();
        switch (err) {
            else => return unexpectedErrorWindows(err),
        }
    }
}

pub fn symLinkWindows(existing_path: []const u8, new_path: []const u8) WindowsSymLinkError!void {
    const existing_path_w = try windows_util.sliceToPrefixedFileW(existing_path);
    const new_path_w = try windows_util.sliceToPrefixedFileW(new_path);
    return symLinkW(&existing_path_w, &new_path_w);
}

pub const PosixSymLinkError = error{
    AccessDenied,
    DiskQuota,
    PathAlreadyExists,
    FileSystem,
    SymLinkLoop,
    NameTooLong,
    FileNotFound,
    SystemResources,
    NoSpaceLeft,
    ReadOnlyFileSystem,
    NotDir,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn symLinkPosixC(existing_path: [*]const u8, new_path: [*]const u8) PosixSymLinkError!void {
    const err = posix.getErrno(posix.symlink(existing_path, new_path));
    switch (err) {
        0 => return,
        posix.EFAULT => unreachable,
        posix.EINVAL => unreachable,
        posix.EACCES => return error.AccessDenied,
        posix.EPERM => return error.AccessDenied,
        posix.EDQUOT => return error.DiskQuota,
        posix.EEXIST => return error.PathAlreadyExists,
        posix.EIO => return error.FileSystem,
        posix.ELOOP => return error.SymLinkLoop,
        posix.ENAMETOOLONG => return error.NameTooLong,
        posix.ENOENT => return error.FileNotFound,
        posix.ENOTDIR => return error.NotDir,
        posix.ENOMEM => return error.SystemResources,
        posix.ENOSPC => return error.NoSpaceLeft,
        posix.EROFS => return error.ReadOnlyFileSystem,
        else => return unexpectedErrorPosix(err),
    }
}

pub fn symLinkPosix(existing_path: []const u8, new_path: []const u8) PosixSymLinkError!void {
    const existing_path_c = try toPosixPath(existing_path);
    const new_path_c = try toPosixPath(new_path);
    return symLinkPosixC(&existing_path_c, &new_path_c);
}

// here we replace the standard +/ with -_ so that it can be used in a file name
const b64_fs_encoder = base64.Base64Encoder.init("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_", base64.standard_pad_char);

/// TODO remove the allocator requirement from this API
pub fn atomicSymLink(allocator: *Allocator, existing_path: []const u8, new_path: []const u8) !void {
    if (symLink(existing_path, new_path)) {
        return;
    } else |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err, // TODO zig should know this set does not include PathAlreadyExists
    }

    const dirname = os.path.dirname(new_path) orelse ".";

    var rand_buf: [12]u8 = undefined;
    const tmp_path = try allocator.alloc(u8, dirname.len + 1 + base64.Base64Encoder.calcSize(rand_buf.len));
    defer allocator.free(tmp_path);
    mem.copy(u8, tmp_path[0..], dirname);
    tmp_path[dirname.len] = os.path.sep;
    while (true) {
        try getRandomBytes(rand_buf[0..]);
        b64_fs_encoder.encode(tmp_path[dirname.len + 1 ..], rand_buf);

        if (symLink(existing_path, tmp_path)) {
            return rename(tmp_path, new_path);
        } else |err| switch (err) {
            error.PathAlreadyExists => continue,
            else => return err, // TODO zig should know this set does not include PathAlreadyExists
        }
    }
}

pub const DeleteFileError = error{
    FileNotFound,
    AccessDenied,
    FileBusy,
    FileSystem,
    IsDir,
    SymLinkLoop,
    NameTooLong,
    NotDir,
    SystemResources,
    ReadOnlyFileSystem,

    /// On Windows, file paths must be valid Unicode.
    InvalidUtf8,

    /// On Windows, file paths cannot contain these characters:
    /// '/', '*', '?', '"', '<', '>', '|'
    BadPathName,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn deleteFile(file_path: []const u8) DeleteFileError!void {
    if (builtin.os == Os.windows) {
        const file_path_w = try windows_util.sliceToPrefixedFileW(file_path);
        return deleteFileW(&file_path_w);
    } else {
        const file_path_c = try toPosixPath(file_path);
        return deleteFileC(&file_path_c);
    }
}

pub fn deleteFileW(file_path: [*]const u16) DeleteFileError!void {
    if (windows.DeleteFileW(file_path) == 0) {
        const err = windows.GetLastError();
        switch (err) {
            windows.ERROR.FILE_NOT_FOUND => return error.FileNotFound,
            windows.ERROR.ACCESS_DENIED => return error.AccessDenied,
            windows.ERROR.FILENAME_EXCED_RANGE => return error.NameTooLong,
            windows.ERROR.INVALID_PARAMETER => return error.NameTooLong,
            else => return unexpectedErrorWindows(err),
        }
    }
}

pub fn deleteFileC(file_path: [*]const u8) DeleteFileError!void {
    if (is_windows) {
        const file_path_w = try windows_util.cStrToPrefixedFileW(file_path);
        return deleteFileW(&file_path_w);
    } else {
        const err = posix.getErrno(posix.unlink(file_path));
        switch (err) {
            0 => return,
            posix.EACCES => return error.AccessDenied,
            posix.EPERM => return error.AccessDenied,
            posix.EBUSY => return error.FileBusy,
            posix.EFAULT => unreachable,
            posix.EINVAL => unreachable,
            posix.EIO => return error.FileSystem,
            posix.EISDIR => return error.IsDir,
            posix.ELOOP => return error.SymLinkLoop,
            posix.ENAMETOOLONG => return error.NameTooLong,
            posix.ENOENT => return error.FileNotFound,
            posix.ENOTDIR => return error.NotDir,
            posix.ENOMEM => return error.SystemResources,
            posix.EROFS => return error.ReadOnlyFileSystem,
            else => return unexpectedErrorPosix(err),
        }
    }
}

/// Guaranteed to be atomic. However until https://patchwork.kernel.org/patch/9636735/ is
/// merged and readily available,
/// there is a possibility of power loss or application termination leaving temporary files present
/// in the same directory as dest_path.
/// Destination file will have the same mode as the source file.
pub fn copyFile(source_path: []const u8, dest_path: []const u8) !void {
    var in_file = try os.File.openRead(source_path);
    defer in_file.close();

    const mode = try in_file.mode();

    var atomic_file = try AtomicFile.init(dest_path, mode);
    defer atomic_file.deinit();

    var buf: [page_size]u8 = undefined;
    while (true) {
        const amt = try in_file.readFull(buf[0..]);
        try atomic_file.file.write(buf[0..amt]);
        if (amt != buf.len) {
            return atomic_file.finish();
        }
    }
}

/// Guaranteed to be atomic. However until https://patchwork.kernel.org/patch/9636735/ is
/// merged and readily available,
/// there is a possibility of power loss or application termination leaving temporary files present
pub fn copyFileMode(source_path: []const u8, dest_path: []const u8, mode: File.Mode) !void {
    var in_file = try os.File.openRead(source_path);
    defer in_file.close();

    var atomic_file = try AtomicFile.init(dest_path, mode);
    defer atomic_file.deinit();

    var buf: [page_size]u8 = undefined;
    while (true) {
        const amt = try in_file.read(buf[0..]);
        try atomic_file.file.write(buf[0..amt]);
        if (amt != buf.len) {
            return atomic_file.finish();
        }
    }
}

pub const AtomicFile = struct {
    file: os.File,
    tmp_path_buf: [MAX_PATH_BYTES]u8,
    dest_path: []const u8,
    finished: bool,

    const InitError = os.File.OpenError;

    /// dest_path must remain valid for the lifetime of AtomicFile
    /// call finish to atomically replace dest_path with contents
    /// TODO once we have null terminated pointers, use the
    /// openWriteNoClobberN function
    pub fn init(dest_path: []const u8, mode: File.Mode) InitError!AtomicFile {
        const dirname = os.path.dirname(dest_path);
        var rand_buf: [12]u8 = undefined;
        const dirname_component_len = if (dirname) |d| d.len + 1 else 0;
        const encoded_rand_len = comptime base64.Base64Encoder.calcSize(rand_buf.len);
        const tmp_path_len = dirname_component_len + encoded_rand_len;
        var tmp_path_buf: [MAX_PATH_BYTES]u8 = undefined;
        if (tmp_path_len >= tmp_path_buf.len) return error.NameTooLong;

        if (dirname) |dir| {
            mem.copy(u8, tmp_path_buf[0..], dir);
            tmp_path_buf[dir.len] = os.path.sep;
        }

        tmp_path_buf[tmp_path_len] = 0;

        while (true) {
            try getRandomBytes(rand_buf[0..]);
            b64_fs_encoder.encode(tmp_path_buf[dirname_component_len..tmp_path_len], rand_buf);

            const file = os.File.openWriteNoClobberC(&tmp_path_buf, mode) catch |err| switch (err) {
                error.PathAlreadyExists => continue,
                // TODO zig should figure out that this error set does not include PathAlreadyExists since
                // it is handled in the above switch
                else => return err,
            };

            return AtomicFile{
                .file = file,
                .tmp_path_buf = tmp_path_buf,
                .dest_path = dest_path,
                .finished = false,
            };
        }
    }

    /// always call deinit, even after successful finish()
    pub fn deinit(self: *AtomicFile) void {
        if (!self.finished) {
            self.file.close();
            deleteFileC(&self.tmp_path_buf) catch {};
            self.finished = true;
        }
    }

    pub fn finish(self: *AtomicFile) !void {
        assert(!self.finished);
        self.file.close();
        self.finished = true;
        if (is_posix) {
            const dest_path_c = try toPosixPath(self.dest_path);
            return renameC(&self.tmp_path_buf, &dest_path_c);
        } else if (is_windows) {
            const dest_path_w = try windows_util.sliceToPrefixedFileW(self.dest_path);
            const tmp_path_w = try windows_util.cStrToPrefixedFileW(&self.tmp_path_buf);
            return renameW(&tmp_path_w, &dest_path_w);
        } else {
            @compileError("Unsupported OS");
        }
    }
};

pub fn renameC(old_path: [*]const u8, new_path: [*]const u8) !void {
    if (is_windows) {
        const old_path_w = try windows_util.cStrToPrefixedFileW(old_path);
        const new_path_w = try windows_util.cStrToPrefixedFileW(new_path);
        return renameW(&old_path_w, &new_path_w);
    } else {
        const err = posix.getErrno(posix.rename(old_path, new_path));
        switch (err) {
            0 => return,
            posix.EACCES => return error.AccessDenied,
            posix.EPERM => return error.AccessDenied,
            posix.EBUSY => return error.FileBusy,
            posix.EDQUOT => return error.DiskQuota,
            posix.EFAULT => unreachable,
            posix.EINVAL => unreachable,
            posix.EISDIR => return error.IsDir,
            posix.ELOOP => return error.SymLinkLoop,
            posix.EMLINK => return error.LinkQuotaExceeded,
            posix.ENAMETOOLONG => return error.NameTooLong,
            posix.ENOENT => return error.FileNotFound,
            posix.ENOTDIR => return error.NotDir,
            posix.ENOMEM => return error.SystemResources,
            posix.ENOSPC => return error.NoSpaceLeft,
            posix.EEXIST => return error.PathAlreadyExists,
            posix.ENOTEMPTY => return error.PathAlreadyExists,
            posix.EROFS => return error.ReadOnlyFileSystem,
            posix.EXDEV => return error.RenameAcrossMountPoints,
            else => return unexpectedErrorPosix(err),
        }
    }
}

pub fn renameW(old_path: [*]const u16, new_path: [*]const u16) !void {
    const flags = windows.MOVEFILE_REPLACE_EXISTING | windows.MOVEFILE_WRITE_THROUGH;
    if (windows.MoveFileExW(old_path, new_path, flags) == 0) {
        const err = windows.GetLastError();
        switch (err) {
            else => return unexpectedErrorWindows(err),
        }
    }
}

pub fn rename(old_path: []const u8, new_path: []const u8) !void {
    if (is_windows) {
        const old_path_w = try windows_util.sliceToPrefixedFileW(old_path);
        const new_path_w = try windows_util.sliceToPrefixedFileW(new_path);
        return renameW(&old_path_w, &new_path_w);
    } else {
        const old_path_c = try toPosixPath(old_path);
        const new_path_c = try toPosixPath(new_path);
        return renameC(&old_path_c, &new_path_c);
    }
}

pub fn makeDir(dir_path: []const u8) !void {
    if (is_windows) {
        return makeDirWindows(dir_path);
    } else {
        return makeDirPosix(dir_path);
    }
}

pub fn makeDirWindows(dir_path: []const u8) !void {
    const dir_path_w = try windows_util.sliceToPrefixedFileW(dir_path);

    if (windows.CreateDirectoryW(&dir_path_w, null) == 0) {
        const err = windows.GetLastError();
        return switch (err) {
            windows.ERROR.ALREADY_EXISTS => error.PathAlreadyExists,
            windows.ERROR.PATH_NOT_FOUND => error.FileNotFound,
            else => unexpectedErrorWindows(err),
        };
    }
}

pub fn makeDirPosixC(dir_path: [*]const u8) !void {
    const err = posix.getErrno(posix.mkdir(dir_path, 0o755));
    switch (err) {
        0 => return,
        posix.EACCES => return error.AccessDenied,
        posix.EPERM => return error.AccessDenied,
        posix.EDQUOT => return error.DiskQuota,
        posix.EEXIST => return error.PathAlreadyExists,
        posix.EFAULT => unreachable,
        posix.ELOOP => return error.SymLinkLoop,
        posix.EMLINK => return error.LinkQuotaExceeded,
        posix.ENAMETOOLONG => return error.NameTooLong,
        posix.ENOENT => return error.FileNotFound,
        posix.ENOMEM => return error.SystemResources,
        posix.ENOSPC => return error.NoSpaceLeft,
        posix.ENOTDIR => return error.NotDir,
        posix.EROFS => return error.ReadOnlyFileSystem,
        else => return unexpectedErrorPosix(err),
    }
}

pub fn makeDirPosix(dir_path: []const u8) !void {
    const dir_path_c = try toPosixPath(dir_path);
    return makeDirPosixC(&dir_path_c);
}

/// Calls makeDir recursively to make an entire path. Returns success if the path
/// already exists and is a directory.
/// TODO determine if we can remove the allocator requirement from this function
pub fn makePath(allocator: *Allocator, full_path: []const u8) !void {
    const resolved_path = try path.resolve(allocator, [][]const u8{full_path});
    defer allocator.free(resolved_path);

    var end_index: usize = resolved_path.len;
    while (true) {
        makeDir(resolved_path[0..end_index]) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // TODO stat the file and return an error if it's not a directory
                // this is important because otherwise a dangling symlink
                // could cause an infinite loop
                if (end_index == resolved_path.len) return;
            },
            error.FileNotFound => {
                // march end_index backward until next path component
                while (true) {
                    end_index -= 1;
                    if (os.path.isSep(resolved_path[end_index])) break;
                }
                continue;
            },
            else => return err,
        };
        if (end_index == resolved_path.len) return;
        // march end_index forward until next path component
        while (true) {
            end_index += 1;
            if (end_index == resolved_path.len or os.path.isSep(resolved_path[end_index])) break;
        }
    }
}

pub const DeleteDirError = error{
    AccessDenied,
    FileBusy,
    SymLinkLoop,
    NameTooLong,
    FileNotFound,
    SystemResources,
    NotDir,
    DirNotEmpty,
    ReadOnlyFileSystem,
    InvalidUtf8,
    BadPathName,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn deleteDirC(dir_path: [*]const u8) DeleteDirError!void {
    switch (builtin.os) {
        Os.windows => {
            const dir_path_w = try windows_util.cStrToPrefixedFileW(dir_path);
            return deleteDirW(&dir_path_w);
        },
        Os.linux, Os.macosx, Os.ios, Os.freebsd, Os.netbsd => {
            const err = posix.getErrno(posix.rmdir(dir_path));
            switch (err) {
                0 => return,
                posix.EACCES => return error.AccessDenied,
                posix.EPERM => return error.AccessDenied,
                posix.EBUSY => return error.FileBusy,
                posix.EFAULT => unreachable,
                posix.EINVAL => unreachable,
                posix.ELOOP => return error.SymLinkLoop,
                posix.ENAMETOOLONG => return error.NameTooLong,
                posix.ENOENT => return error.FileNotFound,
                posix.ENOMEM => return error.SystemResources,
                posix.ENOTDIR => return error.NotDir,
                posix.EEXIST => return error.DirNotEmpty,
                posix.ENOTEMPTY => return error.DirNotEmpty,
                posix.EROFS => return error.ReadOnlyFileSystem,
                else => return unexpectedErrorPosix(err),
            }
        },
        else => @compileError("unimplemented"),
    }
}

pub fn deleteDirW(dir_path_w: [*]const u16) DeleteDirError!void {
    if (windows.RemoveDirectoryW(dir_path_w) == 0) {
        const err = windows.GetLastError();
        switch (err) {
            windows.ERROR.PATH_NOT_FOUND => return error.FileNotFound,
            windows.ERROR.DIR_NOT_EMPTY => return error.DirNotEmpty,
            else => return unexpectedErrorWindows(err),
        }
    }
}

/// Returns ::error.DirNotEmpty if the directory is not empty.
/// To delete a directory recursively, see ::deleteTree
pub fn deleteDir(dir_path: []const u8) DeleteDirError!void {
    switch (builtin.os) {
        Os.windows => {
            const dir_path_w = try windows_util.sliceToPrefixedFileW(dir_path);
            return deleteDirW(&dir_path_w);
        },
        Os.linux, Os.macosx, Os.ios, Os.freebsd, Os.netbsd => {
            const dir_path_c = try toPosixPath(dir_path);
            return deleteDirC(&dir_path_c);
        },
        else => @compileError("unimplemented"),
    }
}

/// Whether ::full_path describes a symlink, file, or directory, this function
/// removes it. If it cannot be removed because it is a non-empty directory,
/// this function recursively removes its entries and then tries again.
const DeleteTreeError = error{
    OutOfMemory,
    AccessDenied,
    FileTooBig,
    IsDir,
    SymLinkLoop,
    ProcessFdQuotaExceeded,
    NameTooLong,
    SystemFdQuotaExceeded,
    NoDevice,
    SystemResources,
    NoSpaceLeft,
    PathAlreadyExists,
    ReadOnlyFileSystem,
    NotDir,
    FileNotFound,
    FileSystem,
    FileBusy,
    DirNotEmpty,
    DeviceBusy,

    /// On Windows, file paths must be valid Unicode.
    InvalidUtf8,

    /// On Windows, file paths cannot contain these characters:
    /// '/', '*', '?', '"', '<', '>', '|'
    BadPathName,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

/// TODO determine if we can remove the allocator requirement
pub fn deleteTree(allocator: *Allocator, full_path: []const u8) DeleteTreeError!void {
    start_over: while (true) {
        var got_access_denied = false;
        // First, try deleting the item as a file. This way we don't follow sym links.
        if (deleteFile(full_path)) {
            return;
        } else |err| switch (err) {
            error.FileNotFound => return,
            error.IsDir => {},
            error.AccessDenied => got_access_denied = true,

            error.InvalidUtf8,
            error.SymLinkLoop,
            error.NameTooLong,
            error.SystemResources,
            error.ReadOnlyFileSystem,
            error.NotDir,
            error.FileSystem,
            error.FileBusy,
            error.BadPathName,
            error.Unexpected,
            => return err,
        }
        {
            var dir = Dir.open(allocator, full_path) catch |err| switch (err) {
                error.NotDir => {
                    if (got_access_denied) {
                        return error.AccessDenied;
                    }
                    continue :start_over;
                },

                error.OutOfMemory,
                error.AccessDenied,
                error.FileTooBig,
                error.IsDir,
                error.SymLinkLoop,
                error.ProcessFdQuotaExceeded,
                error.NameTooLong,
                error.SystemFdQuotaExceeded,
                error.NoDevice,
                error.FileNotFound,
                error.SystemResources,
                error.NoSpaceLeft,
                error.PathAlreadyExists,
                error.Unexpected,
                error.InvalidUtf8,
                error.BadPathName,
                error.DeviceBusy,
                => return err,
            };
            defer dir.close();

            var full_entry_buf = ArrayList(u8).init(allocator);
            defer full_entry_buf.deinit();

            while (try dir.next()) |entry| {
                try full_entry_buf.resize(full_path.len + entry.name.len + 1);
                const full_entry_path = full_entry_buf.toSlice();
                mem.copy(u8, full_entry_path, full_path);
                full_entry_path[full_path.len] = path.sep;
                mem.copy(u8, full_entry_path[full_path.len + 1 ..], entry.name);

                try deleteTree(allocator, full_entry_path);
            }
        }
        return deleteDir(full_path);
    }
}

pub const Dir = struct {
    handle: Handle,
    allocator: *Allocator,

    pub const Handle = switch (builtin.os) {
        Os.macosx, Os.ios, Os.freebsd, Os.netbsd => struct {
            fd: i32,
            seek: i64,
            buf: []u8,
            index: usize,
            end_index: usize,
        },
        Os.linux => struct {
            fd: i32,
            buf: []u8,
            index: usize,
            end_index: usize,
        },
        Os.windows => struct {
            handle: windows.HANDLE,
            find_file_data: windows.WIN32_FIND_DATAW,
            first: bool,
            name_data: [256]u8,
        },
        else => @compileError("unimplemented"),
    };

    pub const Entry = struct {
        name: []const u8,
        kind: Kind,

        pub const Kind = enum {
            BlockDevice,
            CharacterDevice,
            Directory,
            NamedPipe,
            SymLink,
            File,
            UnixDomainSocket,
            Whiteout,
            Unknown,
        };
    };

    pub const OpenError = error{
        FileNotFound,
        NotDir,
        AccessDenied,
        FileTooBig,
        IsDir,
        SymLinkLoop,
        ProcessFdQuotaExceeded,
        NameTooLong,
        SystemFdQuotaExceeded,
        NoDevice,
        SystemResources,
        NoSpaceLeft,
        PathAlreadyExists,
        OutOfMemory,
        InvalidUtf8,
        BadPathName,
        DeviceBusy,

        /// See https://github.com/ziglang/zig/issues/1396
        Unexpected,
    };

    /// TODO remove the allocator requirement from this API
    pub fn open(allocator: *Allocator, dir_path: []const u8) OpenError!Dir {
        return Dir{
            .allocator = allocator,
            .handle = switch (builtin.os) {
                Os.windows => blk: {
                    var find_file_data: windows.WIN32_FIND_DATAW = undefined;
                    const handle = try windows_util.windowsFindFirstFile(dir_path, &find_file_data);
                    break :blk Handle{
                        .handle = handle,
                        .find_file_data = find_file_data, // TODO guaranteed copy elision
                        .first = true,
                        .name_data = undefined,
                    };
                },
                Os.macosx, Os.ios, Os.freebsd, Os.netbsd => Handle{
                    .fd = try posixOpen(
                        dir_path,
                        posix.O_RDONLY | posix.O_NONBLOCK | posix.O_DIRECTORY | posix.O_CLOEXEC,
                        0,
                    ),
                    .seek = 0,
                    .index = 0,
                    .end_index = 0,
                    .buf = []u8{},
                },
                Os.linux => Handle{
                    .fd = try posixOpen(
                        dir_path,
                        posix.O_RDONLY | posix.O_DIRECTORY | posix.O_CLOEXEC,
                        0,
                    ),
                    .index = 0,
                    .end_index = 0,
                    .buf = []u8{},
                },
                else => @compileError("unimplemented"),
            },
        };
    }

    pub fn close(self: *Dir) void {
        switch (builtin.os) {
            Os.windows => {
                _ = windows.FindClose(self.handle.handle);
            },
            Os.macosx, Os.ios, Os.linux, Os.freebsd, Os.netbsd => {
                self.allocator.free(self.handle.buf);
                os.close(self.handle.fd);
            },
            else => @compileError("unimplemented"),
        }
    }

    /// Memory such as file names referenced in this returned entry becomes invalid
    /// with subsequent calls to next, as well as when this `Dir` is deinitialized.
    pub fn next(self: *Dir) !?Entry {
        switch (builtin.os) {
            Os.linux => return self.nextLinux(),
            Os.macosx, Os.ios => return self.nextDarwin(),
            Os.windows => return self.nextWindows(),
            Os.freebsd => return self.nextFreebsd(),
            Os.netbsd => return self.nextFreebsd(),
            else => @compileError("unimplemented"),
        }
    }

    fn nextDarwin(self: *Dir) !?Entry {
        start_over: while (true) {
            if (self.handle.index >= self.handle.end_index) {
                if (self.handle.buf.len == 0) {
                    self.handle.buf = try self.allocator.alloc(u8, page_size);
                }

                while (true) {
                    const result = posix.getdirentries64(self.handle.fd, self.handle.buf.ptr, self.handle.buf.len, &self.handle.seek);
                    const err = posix.getErrno(result);
                    if (err > 0) {
                        switch (err) {
                            posix.EBADF, posix.EFAULT, posix.ENOTDIR => unreachable,
                            posix.EINVAL => {
                                self.handle.buf = try self.allocator.realloc(self.handle.buf, self.handle.buf.len * 2);
                                continue;
                            },
                            else => return unexpectedErrorPosix(err),
                        }
                    }
                    if (result == 0) return null;
                    self.handle.index = 0;
                    self.handle.end_index = result;
                    break;
                }
            }
            const darwin_entry = @ptrCast(*align(1) posix.dirent, &self.handle.buf[self.handle.index]);
            const next_index = self.handle.index + darwin_entry.d_reclen;
            self.handle.index = next_index;

            const name = @ptrCast([*]u8, &darwin_entry.d_name)[0..darwin_entry.d_namlen];

            if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) {
                continue :start_over;
            }

            const entry_kind = switch (darwin_entry.d_type) {
                posix.DT_BLK => Entry.Kind.BlockDevice,
                posix.DT_CHR => Entry.Kind.CharacterDevice,
                posix.DT_DIR => Entry.Kind.Directory,
                posix.DT_FIFO => Entry.Kind.NamedPipe,
                posix.DT_LNK => Entry.Kind.SymLink,
                posix.DT_REG => Entry.Kind.File,
                posix.DT_SOCK => Entry.Kind.UnixDomainSocket,
                posix.DT_WHT => Entry.Kind.Whiteout,
                else => Entry.Kind.Unknown,
            };
            return Entry{
                .name = name,
                .kind = entry_kind,
            };
        }
    }

    fn nextWindows(self: *Dir) !?Entry {
        while (true) {
            if (self.handle.first) {
                self.handle.first = false;
            } else {
                if (!try windows_util.windowsFindNextFile(self.handle.handle, &self.handle.find_file_data))
                    return null;
            }
            const name_utf16le = mem.toSlice(u16, self.handle.find_file_data.cFileName[0..].ptr);
            if (mem.eql(u16, name_utf16le, []u16{'.'}) or mem.eql(u16, name_utf16le, []u16{ '.', '.' }))
                continue;
            // Trust that Windows gives us valid UTF-16LE
            const name_utf8_len = std.unicode.utf16leToUtf8(self.handle.name_data[0..], name_utf16le) catch unreachable;
            const name_utf8 = self.handle.name_data[0..name_utf8_len];
            const kind = blk: {
                const attrs = self.handle.find_file_data.dwFileAttributes;
                if (attrs & windows.FILE_ATTRIBUTE_DIRECTORY != 0) break :blk Entry.Kind.Directory;
                if (attrs & windows.FILE_ATTRIBUTE_REPARSE_POINT != 0) break :blk Entry.Kind.SymLink;
                if (attrs & windows.FILE_ATTRIBUTE_NORMAL != 0) break :blk Entry.Kind.File;
                break :blk Entry.Kind.Unknown;
            };
            return Entry{
                .name = name_utf8,
                .kind = kind,
            };
        }
    }

    fn nextLinux(self: *Dir) !?Entry {
        start_over: while (true) {
            if (self.handle.index >= self.handle.end_index) {
                if (self.handle.buf.len == 0) {
                    self.handle.buf = try self.allocator.alloc(u8, page_size);
                }

                while (true) {
                    const result = posix.getdents64(self.handle.fd, self.handle.buf.ptr, self.handle.buf.len);
                    const err = posix.getErrno(result);
                    if (err > 0) {
                        switch (err) {
                            posix.EBADF, posix.EFAULT, posix.ENOTDIR => unreachable,
                            posix.EINVAL => {
                                self.handle.buf = try self.allocator.realloc(self.handle.buf, self.handle.buf.len * 2);
                                continue;
                            },
                            else => return unexpectedErrorPosix(err),
                        }
                    }
                    if (result == 0) return null;
                    self.handle.index = 0;
                    self.handle.end_index = result;
                    break;
                }
            }
            const linux_entry = @ptrCast(*align(1) posix.dirent64, &self.handle.buf[self.handle.index]);
            const next_index = self.handle.index + linux_entry.d_reclen;
            self.handle.index = next_index;

            const name = cstr.toSlice(@ptrCast([*]u8, &linux_entry.d_name));

            // skip . and .. entries
            if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) {
                continue :start_over;
            }

            const entry_kind = switch (linux_entry.d_type) {
                posix.DT_BLK => Entry.Kind.BlockDevice,
                posix.DT_CHR => Entry.Kind.CharacterDevice,
                posix.DT_DIR => Entry.Kind.Directory,
                posix.DT_FIFO => Entry.Kind.NamedPipe,
                posix.DT_LNK => Entry.Kind.SymLink,
                posix.DT_REG => Entry.Kind.File,
                posix.DT_SOCK => Entry.Kind.UnixDomainSocket,
                else => Entry.Kind.Unknown,
            };
            return Entry{
                .name = name,
                .kind = entry_kind,
            };
        }
    }

    fn nextFreebsd(self: *Dir) !?Entry {
        start_over: while (true) {
            if (self.handle.index >= self.handle.end_index) {
                if (self.handle.buf.len == 0) {
                    self.handle.buf = try self.allocator.alloc(u8, page_size);
                }

                while (true) {
                    const result = posix.getdirentries(self.handle.fd, self.handle.buf.ptr, self.handle.buf.len, &self.handle.seek);
                    const err = posix.getErrno(result);
                    if (err > 0) {
                        switch (err) {
                            posix.EBADF, posix.EFAULT, posix.ENOTDIR => unreachable,
                            posix.EINVAL => {
                                self.handle.buf = try self.allocator.realloc(self.handle.buf, self.handle.buf.len * 2);
                                continue;
                            },
                            else => return unexpectedErrorPosix(err),
                        }
                    }
                    if (result == 0) return null;
                    self.handle.index = 0;
                    self.handle.end_index = result;
                    break;
                }
            }
            const freebsd_entry = @ptrCast(*align(1) posix.dirent, &self.handle.buf[self.handle.index]);
            const next_index = self.handle.index + freebsd_entry.d_reclen;
            self.handle.index = next_index;

            const name = @ptrCast([*]u8, &freebsd_entry.d_name)[0..freebsd_entry.d_namlen];

            if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) {
                continue :start_over;
            }

            const entry_kind = switch (freebsd_entry.d_type) {
                posix.DT_BLK => Entry.Kind.BlockDevice,
                posix.DT_CHR => Entry.Kind.CharacterDevice,
                posix.DT_DIR => Entry.Kind.Directory,
                posix.DT_FIFO => Entry.Kind.NamedPipe,
                posix.DT_LNK => Entry.Kind.SymLink,
                posix.DT_REG => Entry.Kind.File,
                posix.DT_SOCK => Entry.Kind.UnixDomainSocket,
                posix.DT_WHT => Entry.Kind.Whiteout,
                else => Entry.Kind.Unknown,
            };
            return Entry{
                .name = name,
                .kind = entry_kind,
            };
        }
    }
};

pub fn changeCurDir(dir_path: []const u8) !void {
    const dir_path_c = try toPosixPath(dir_path);
    const err = posix.getErrno(posix.chdir(&dir_path_c));
    switch (err) {
        0 => return,
        posix.EACCES => return error.AccessDenied,
        posix.EFAULT => unreachable,
        posix.EIO => return error.FileSystem,
        posix.ELOOP => return error.SymLinkLoop,
        posix.ENAMETOOLONG => return error.NameTooLong,
        posix.ENOENT => return error.FileNotFound,
        posix.ENOMEM => return error.SystemResources,
        posix.ENOTDIR => return error.NotDir,
        else => return unexpectedErrorPosix(err),
    }
}

/// Read value of a symbolic link.
/// The return value is a slice of out_buffer.
pub fn readLinkC(out_buffer: *[posix.PATH_MAX]u8, pathname: [*]const u8) ![]u8 {
    const rc = posix.readlink(pathname, out_buffer, out_buffer.len);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return out_buffer[0..rc],
        posix.EACCES => return error.AccessDenied,
        posix.EFAULT => unreachable,
        posix.EINVAL => unreachable,
        posix.EIO => return error.FileSystem,
        posix.ELOOP => return error.SymLinkLoop,
        posix.ENAMETOOLONG => unreachable, // out_buffer is at least PATH_MAX
        posix.ENOENT => return error.FileNotFound,
        posix.ENOMEM => return error.SystemResources,
        posix.ENOTDIR => return error.NotDir,
        else => return unexpectedErrorPosix(err),
    }
}

/// Read value of a symbolic link.
/// The return value is a slice of out_buffer.
pub fn readLink(out_buffer: *[posix.PATH_MAX]u8, file_path: []const u8) ![]u8 {
    const file_path_c = try toPosixPath(file_path);
    return readLinkC(out_buffer, &file_path_c);
}

pub fn posix_setuid(uid: u32) !void {
    const err = posix.getErrno(posix.setuid(uid));
    if (err == 0) return;
    return switch (err) {
        posix.EAGAIN => error.ResourceLimitReached,
        posix.EINVAL => error.InvalidUserId,
        posix.EPERM => error.PermissionDenied,
        else => unexpectedErrorPosix(err),
    };
}

pub fn posix_setreuid(ruid: u32, euid: u32) !void {
    const err = posix.getErrno(posix.setreuid(ruid, euid));
    if (err == 0) return;
    return switch (err) {
        posix.EAGAIN => error.ResourceLimitReached,
        posix.EINVAL => error.InvalidUserId,
        posix.EPERM => error.PermissionDenied,
        else => unexpectedErrorPosix(err),
    };
}

pub fn posix_setgid(gid: u32) !void {
    const err = posix.getErrno(posix.setgid(gid));
    if (err == 0) return;
    return switch (err) {
        posix.EAGAIN => error.ResourceLimitReached,
        posix.EINVAL => error.InvalidUserId,
        posix.EPERM => error.PermissionDenied,
        else => unexpectedErrorPosix(err),
    };
}

pub fn posix_setregid(rgid: u32, egid: u32) !void {
    const err = posix.getErrno(posix.setregid(rgid, egid));
    if (err == 0) return;
    return switch (err) {
        posix.EAGAIN => error.ResourceLimitReached,
        posix.EINVAL => error.InvalidUserId,
        posix.EPERM => error.PermissionDenied,
        else => unexpectedErrorPosix(err),
    };
}

pub const WindowsGetStdHandleErrs = error{
    NoStdHandles,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn windowsGetStdHandle(handle_id: windows.DWORD) WindowsGetStdHandleErrs!windows.HANDLE {
    if (windows.GetStdHandle(handle_id)) |handle| {
        if (handle == windows.INVALID_HANDLE_VALUE) {
            const err = windows.GetLastError();
            return switch (err) {
                else => os.unexpectedErrorWindows(err),
            };
        }
        return handle;
    } else {
        return error.NoStdHandles;
    }
}

pub const ArgIteratorPosix = struct {
    index: usize,
    count: usize,

    pub fn init() ArgIteratorPosix {
        return ArgIteratorPosix{
            .index = 0,
            .count = raw.len,
        };
    }

    pub fn next(self: *ArgIteratorPosix) ?[]const u8 {
        if (self.index == self.count) return null;

        const s = raw[self.index];
        self.index += 1;
        return cstr.toSlice(s);
    }

    pub fn skip(self: *ArgIteratorPosix) bool {
        if (self.index == self.count) return false;

        self.index += 1;
        return true;
    }

    /// This is marked as public but actually it's only meant to be used
    /// internally by zig's startup code.
    pub var raw: [][*]u8 = undefined;
};

pub const ArgIteratorWindows = struct {
    index: usize,
    cmd_line: [*]const u8,
    in_quote: bool,
    quote_count: usize,
    seen_quote_count: usize,

    pub const NextError = error{OutOfMemory};

    pub fn init() ArgIteratorWindows {
        return initWithCmdLine(windows.GetCommandLineA());
    }

    pub fn initWithCmdLine(cmd_line: [*]const u8) ArgIteratorWindows {
        return ArgIteratorWindows{
            .index = 0,
            .cmd_line = cmd_line,
            .in_quote = false,
            .quote_count = countQuotes(cmd_line),
            .seen_quote_count = 0,
        };
    }

    /// You must free the returned memory when done.
    pub fn next(self: *ArgIteratorWindows, allocator: *Allocator) ?(NextError![]u8) {
        // march forward over whitespace
        while (true) : (self.index += 1) {
            const byte = self.cmd_line[self.index];
            switch (byte) {
                0 => return null,
                ' ', '\t' => continue,
                else => break,
            }
        }

        return self.internalNext(allocator);
    }

    pub fn skip(self: *ArgIteratorWindows) bool {
        // march forward over whitespace
        while (true) : (self.index += 1) {
            const byte = self.cmd_line[self.index];
            switch (byte) {
                0 => return false,
                ' ', '\t' => continue,
                else => break,
            }
        }

        var backslash_count: usize = 0;
        while (true) : (self.index += 1) {
            const byte = self.cmd_line[self.index];
            switch (byte) {
                0 => return true,
                '"' => {
                    const quote_is_real = backslash_count % 2 == 0;
                    if (quote_is_real) {
                        self.seen_quote_count += 1;
                    }
                },
                '\\' => {
                    backslash_count += 1;
                },
                ' ', '\t' => {
                    if (self.seen_quote_count % 2 == 0 or self.seen_quote_count == self.quote_count) {
                        return true;
                    }
                    backslash_count = 0;
                },
                else => {
                    backslash_count = 0;
                    continue;
                },
            }
        }
    }

    fn internalNext(self: *ArgIteratorWindows, allocator: *Allocator) NextError![]u8 {
        var buf = try Buffer.initSize(allocator, 0);
        defer buf.deinit();

        var backslash_count: usize = 0;
        while (true) : (self.index += 1) {
            const byte = self.cmd_line[self.index];
            switch (byte) {
                0 => return buf.toOwnedSlice(),
                '"' => {
                    const quote_is_real = backslash_count % 2 == 0;
                    try self.emitBackslashes(&buf, backslash_count / 2);
                    backslash_count = 0;

                    if (quote_is_real) {
                        self.seen_quote_count += 1;
                        if (self.seen_quote_count == self.quote_count and self.seen_quote_count % 2 == 1) {
                            try buf.appendByte('"');
                        }
                    } else {
                        try buf.appendByte('"');
                    }
                },
                '\\' => {
                    backslash_count += 1;
                },
                ' ', '\t' => {
                    try self.emitBackslashes(&buf, backslash_count);
                    backslash_count = 0;
                    if (self.seen_quote_count % 2 == 1 and self.seen_quote_count != self.quote_count) {
                        try buf.appendByte(byte);
                    } else {
                        return buf.toOwnedSlice();
                    }
                },
                else => {
                    try self.emitBackslashes(&buf, backslash_count);
                    backslash_count = 0;
                    try buf.appendByte(byte);
                },
            }
        }
    }

    fn emitBackslashes(self: *ArgIteratorWindows, buf: *Buffer, emit_count: usize) !void {
        var i: usize = 0;
        while (i < emit_count) : (i += 1) {
            try buf.appendByte('\\');
        }
    }

    fn countQuotes(cmd_line: [*]const u8) usize {
        var result: usize = 0;
        var backslash_count: usize = 0;
        var index: usize = 0;
        while (true) : (index += 1) {
            const byte = cmd_line[index];
            switch (byte) {
                0 => return result,
                '\\' => backslash_count += 1,
                '"' => {
                    result += 1 - (backslash_count % 2);
                    backslash_count = 0;
                },
                else => {
                    backslash_count = 0;
                },
            }
        }
    }
};

pub const ArgIterator = struct {
    const InnerType = if (builtin.os == Os.windows) ArgIteratorWindows else ArgIteratorPosix;

    inner: InnerType,

    pub fn init() ArgIterator {
        return ArgIterator{ .inner = InnerType.init() };
    }

    pub const NextError = ArgIteratorWindows.NextError;

    /// You must free the returned memory when done.
    pub fn next(self: *ArgIterator, allocator: *Allocator) ?(NextError![]u8) {
        if (builtin.os == Os.windows) {
            return self.inner.next(allocator);
        } else {
            return mem.dupe(allocator, u8, self.inner.next() orelse return null);
        }
    }

    /// If you only are targeting posix you can call this and not need an allocator.
    pub fn nextPosix(self: *ArgIterator) ?[]const u8 {
        return self.inner.next();
    }

    /// Parse past 1 argument without capturing it.
    /// Returns `true` if skipped an arg, `false` if we are at the end.
    pub fn skip(self: *ArgIterator) bool {
        return self.inner.skip();
    }
};

pub fn args() ArgIterator {
    return ArgIterator.init();
}

/// Caller must call argsFree on result.
pub fn argsAlloc(allocator: *mem.Allocator) ![]const []u8 {
    // TODO refactor to only make 1 allocation.
    var it = args();
    var contents = try Buffer.initSize(allocator, 0);
    defer contents.deinit();

    var slice_list = ArrayList(usize).init(allocator);
    defer slice_list.deinit();

    while (it.next(allocator)) |arg_or_err| {
        const arg = try arg_or_err;
        defer allocator.free(arg);
        try contents.append(arg);
        try slice_list.append(arg.len);
    }

    const contents_slice = contents.toSliceConst();
    const slice_sizes = slice_list.toSliceConst();
    const slice_list_bytes = try math.mul(usize, @sizeOf([]u8), slice_sizes.len);
    const total_bytes = try math.add(usize, slice_list_bytes, contents_slice.len);
    const buf = try allocator.alignedAlloc(u8, @alignOf([]u8), total_bytes);
    errdefer allocator.free(buf);

    const result_slice_list = @bytesToSlice([]u8, buf[0..slice_list_bytes]);
    const result_contents = buf[slice_list_bytes..];
    mem.copy(u8, result_contents, contents_slice);

    var contents_index: usize = 0;
    for (slice_sizes) |len, i| {
        const new_index = contents_index + len;
        result_slice_list[i] = result_contents[contents_index..new_index];
        contents_index = new_index;
    }

    return result_slice_list;
}

pub fn argsFree(allocator: *mem.Allocator, args_alloc: []const []u8) void {
    var total_bytes: usize = 0;
    for (args_alloc) |arg| {
        total_bytes += @sizeOf([]u8) + arg.len;
    }
    const unaligned_allocated_buf = @ptrCast([*]const u8, args_alloc.ptr)[0..total_bytes];
    const aligned_allocated_buf = @alignCast(@alignOf([]u8), unaligned_allocated_buf);
    return allocator.free(aligned_allocated_buf);
}

test "windows arg parsing" {
    testWindowsCmdLine(c"a   b\tc d", [][]const u8{ "a", "b", "c", "d" });
    testWindowsCmdLine(c"\"abc\" d e", [][]const u8{ "abc", "d", "e" });
    testWindowsCmdLine(c"a\\\\\\b d\"e f\"g h", [][]const u8{ "a\\\\\\b", "de fg", "h" });
    testWindowsCmdLine(c"a\\\\\\\"b c d", [][]const u8{ "a\\\"b", "c", "d" });
    testWindowsCmdLine(c"a\\\\\\\\\"b c\" d e", [][]const u8{ "a\\\\b c", "d", "e" });
    testWindowsCmdLine(c"a   b\tc \"d f", [][]const u8{ "a", "b", "c", "\"d", "f" });

    testWindowsCmdLine(c"\".\\..\\zig-cache\\build\" \"bin\\zig.exe\" \".\\..\" \".\\..\\zig-cache\" \"--help\"", [][]const u8{
        ".\\..\\zig-cache\\build",
        "bin\\zig.exe",
        ".\\..",
        ".\\..\\zig-cache",
        "--help",
    });
}

fn testWindowsCmdLine(input_cmd_line: [*]const u8, expected_args: []const []const u8) void {
    var it = ArgIteratorWindows.initWithCmdLine(input_cmd_line);
    for (expected_args) |expected_arg| {
        const arg = it.next(debug.global_allocator).? catch unreachable;
        testing.expectEqualSlices(u8, expected_arg, arg);
    }
    testing.expect(it.next(debug.global_allocator) == null);
}

// TODO make this a build variable that you can set
const unexpected_error_tracing = false;
const UnexpectedError = error{
    /// The Operating System returned an undocumented error code.
    Unexpected,
};

/// Call this when you made a syscall or something that sets errno
/// and you get an unexpected error.
pub fn unexpectedErrorPosix(errno: usize) UnexpectedError {
    if (unexpected_error_tracing) {
        debug.warn("unexpected errno: {}\n", errno);
        debug.dumpCurrentStackTrace(null);
    }
    return error.Unexpected;
}

/// Call this when you made a windows DLL call or something that does SetLastError
/// and you get an unexpected error.
pub fn unexpectedErrorWindows(err: windows.DWORD) UnexpectedError {
    if (unexpected_error_tracing) {
        debug.warn("unexpected GetLastError(): {}\n", err);
        @breakpoint();
        debug.dumpCurrentStackTrace(null);
    }
    return error.Unexpected;
}

pub fn openSelfExe() !os.File {
    switch (builtin.os) {
        Os.linux => return os.File.openReadC(c"/proc/self/exe"),
        Os.macosx, Os.ios, Os.freebsd, Os.netbsd => {
            var buf: [MAX_PATH_BYTES]u8 = undefined;
            const self_exe_path = try selfExePath(&buf);
            buf[self_exe_path.len] = 0;
            return os.File.openReadC(self_exe_path.ptr);
        },
        Os.windows => {
            var buf: [windows_util.PATH_MAX_WIDE]u16 = undefined;
            const wide_slice = try selfExePathW(&buf);
            return os.File.openReadW(wide_slice.ptr);
        },
        else => @compileError("Unsupported OS"),
    }
}

test "openSelfExe" {
    switch (builtin.os) {
        Os.linux, Os.macosx, Os.ios, Os.windows, Os.freebsd => (try openSelfExe()).close(),
        else => return error.SkipZigTest, // Unsupported OS.
    }
}

pub fn selfExePathW(out_buffer: *[windows_util.PATH_MAX_WIDE]u16) ![]u16 {
    const casted_len = @intCast(windows.DWORD, out_buffer.len); // TODO shouldn't need this cast
    const rc = windows.GetModuleFileNameW(null, out_buffer, casted_len);
    assert(rc <= out_buffer.len);
    if (rc == 0) {
        const err = windows.GetLastError();
        switch (err) {
            else => return unexpectedErrorWindows(err),
        }
    }
    return out_buffer[0..rc];
}

/// Get the path to the current executable.
/// If you only need the directory, use selfExeDirPath.
/// If you only want an open file handle, use openSelfExe.
/// This function may return an error if the current executable
/// was deleted after spawning.
/// Returned value is a slice of out_buffer.
///
/// On Linux, depends on procfs being mounted. If the currently executing binary has
/// been deleted, the file path looks something like `/a/b/c/exe (deleted)`.
/// TODO make the return type of this a null terminated pointer
pub fn selfExePath(out_buffer: *[MAX_PATH_BYTES]u8) ![]u8 {
    switch (builtin.os) {
        Os.linux => return readLink(out_buffer, "/proc/self/exe"),
        Os.freebsd => {
            var mib = [4]c_int{ posix.CTL_KERN, posix.KERN_PROC, posix.KERN_PROC_PATHNAME, -1 };
            var out_len: usize = out_buffer.len;
            const err = posix.getErrno(posix.sysctl(&mib, 4, out_buffer, &out_len, null, 0));

            if (err == 0) return mem.toSlice(u8, out_buffer);

            return switch (err) {
                posix.EFAULT => error.BadAdress,
                posix.EPERM => error.PermissionDenied,
                else => unexpectedErrorPosix(err),
            };
        },
        Os.netbsd => {
            var mib = [4]c_int{ posix.CTL_KERN, posix.KERN_PROC_ARGS, -1, posix.KERN_PROC_PATHNAME };
            var out_len: usize = out_buffer.len;
            const err = posix.getErrno(posix.sysctl(&mib, 4, out_buffer, &out_len, null, 0));

            if (err == 0) return mem.toSlice(u8, out_buffer);

            return switch (err) {
                posix.EFAULT => error.BadAdress,
                posix.EPERM => error.PermissionDenied,
                else => unexpectedErrorPosix(err),
            };
        },
        Os.windows => {
            var utf16le_buf: [windows_util.PATH_MAX_WIDE]u16 = undefined;
            const utf16le_slice = try selfExePathW(&utf16le_buf);
            // Trust that Windows gives us valid UTF-16LE.
            const end_index = std.unicode.utf16leToUtf8(out_buffer, utf16le_slice) catch unreachable;
            return out_buffer[0..end_index];
        },
        Os.macosx, Os.ios => {
            var u32_len: u32 = @intCast(u32, out_buffer.len); // TODO shouldn't need this cast
            const rc = c._NSGetExecutablePath(out_buffer, &u32_len);
            if (rc != 0) return error.NameTooLong;
            return mem.toSlice(u8, out_buffer);
        },
        else => @compileError("Unsupported OS"),
    }
}

/// `selfExeDirPath` except allocates the result on the heap.
/// Caller owns returned memory.
pub fn selfExeDirPathAlloc(allocator: *Allocator) ![]u8 {
    var buf: [MAX_PATH_BYTES]u8 = undefined;
    return mem.dupe(allocator, u8, try selfExeDirPath(&buf));
}

/// Get the directory path that contains the current executable.
/// Returned value is a slice of out_buffer.
pub fn selfExeDirPath(out_buffer: *[MAX_PATH_BYTES]u8) ![]const u8 {
    switch (builtin.os) {
        Os.linux => {
            // If the currently executing binary has been deleted,
            // the file path looks something like `/a/b/c/exe (deleted)`
            // This path cannot be opened, but it's valid for determining the directory
            // the executable was in when it was run.
            const full_exe_path = try readLinkC(out_buffer, c"/proc/self/exe");
            // Assume that /proc/self/exe has an absolute path, and therefore dirname
            // will not return null.
            return path.dirname(full_exe_path).?;
        },
        Os.windows, Os.macosx, Os.ios, Os.freebsd, Os.netbsd => {
            const self_exe_path = try selfExePath(out_buffer);
            // Assume that the OS APIs return absolute paths, and therefore dirname
            // will not return null.
            return path.dirname(self_exe_path).?;
        },
        else => @compileError("Unsupported OS"),
    }
}

pub fn isTty(handle: FileHandle) bool {
    if (is_windows) {
        return windows_util.windowsIsTty(handle);
    } else {
        if (builtin.link_libc) {
            return c.isatty(handle) != 0;
        } else {
            return posix.isatty(handle);
        }
    }
}

pub fn supportsAnsiEscapeCodes(handle: FileHandle) bool {
    if (is_windows) {
        return windows_util.windowsIsCygwinPty(handle);
    } else {
        if (builtin.link_libc) {
            return c.isatty(handle) != 0;
        } else {
            return posix.isatty(handle);
        }
    }
}

pub const PosixSocketError = error{
    /// Permission to create a socket of the specified type and/or
    /// pro‐tocol is denied.
    PermissionDenied,

    /// The implementation does not support the specified address family.
    AddressFamilyNotSupported,

    /// Unknown protocol, or protocol family not available.
    ProtocolFamilyNotAvailable,

    /// The per-process limit on the number of open file descriptors has been reached.
    ProcessFdQuotaExceeded,

    /// The system-wide limit on the total number of open files has been reached.
    SystemFdQuotaExceeded,

    /// Insufficient memory is available. The socket cannot be created until sufficient
    /// resources are freed.
    SystemResources,

    /// The protocol type or the specified protocol is not supported within this domain.
    ProtocolNotSupported,
};

pub fn posixSocket(domain: u32, socket_type: u32, protocol: u32) !i32 {
    const rc = posix.socket(domain, socket_type, protocol);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        posix.EACCES => return PosixSocketError.PermissionDenied,
        posix.EAFNOSUPPORT => return PosixSocketError.AddressFamilyNotSupported,
        posix.EINVAL => return PosixSocketError.ProtocolFamilyNotAvailable,
        posix.EMFILE => return PosixSocketError.ProcessFdQuotaExceeded,
        posix.ENFILE => return PosixSocketError.SystemFdQuotaExceeded,
        posix.ENOBUFS, posix.ENOMEM => return PosixSocketError.SystemResources,
        posix.EPROTONOSUPPORT => return PosixSocketError.ProtocolNotSupported,
        else => return unexpectedErrorPosix(err),
    }
}

pub const PosixBindError = error{
    /// The address is protected, and the user is not the superuser.
    /// For UNIX domain sockets: Search permission is denied on  a  component
    /// of  the  path  prefix.
    AccessDenied,

    /// The given address is already in use, or in the case of Internet domain sockets,
    /// The  port number was specified as zero in the socket
    /// address structure, but, upon attempting to bind to  an  ephemeral  port,  it  was
    /// determined  that  all  port  numbers in the ephemeral port range are currently in
    /// use.  See the discussion of /proc/sys/net/ipv4/ip_local_port_range ip(7).
    AddressInUse,

    /// A nonexistent interface was requested or the requested address was not local.
    AddressNotAvailable,

    /// Too many symbolic links were encountered in resolving addr.
    SymLinkLoop,

    /// addr is too long.
    NameTooLong,

    /// A component in the directory prefix of the socket pathname does not exist.
    FileNotFound,

    /// Insufficient kernel memory was available.
    SystemResources,

    /// A component of the path prefix is not a directory.
    NotDir,

    /// The socket inode would reside on a read-only filesystem.
    ReadOnlyFileSystem,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

/// addr is `&const T` where T is one of the sockaddr
pub fn posixBind(fd: i32, addr: *const posix.sockaddr) PosixBindError!void {
    const rc = posix.bind(fd, addr, @sizeOf(posix.sockaddr));
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return,
        posix.EACCES => return PosixBindError.AccessDenied,
        posix.EADDRINUSE => return PosixBindError.AddressInUse,
        posix.EBADF => unreachable, // always a race condition if this error is returned
        posix.EINVAL => unreachable,
        posix.ENOTSOCK => unreachable,
        posix.EADDRNOTAVAIL => return PosixBindError.AddressNotAvailable,
        posix.EFAULT => unreachable,
        posix.ELOOP => return PosixBindError.SymLinkLoop,
        posix.ENAMETOOLONG => return PosixBindError.NameTooLong,
        posix.ENOENT => return PosixBindError.FileNotFound,
        posix.ENOMEM => return PosixBindError.SystemResources,
        posix.ENOTDIR => return PosixBindError.NotDir,
        posix.EROFS => return PosixBindError.ReadOnlyFileSystem,
        else => return unexpectedErrorPosix(err),
    }
}

const PosixListenError = error{
    /// Another socket is already listening on the same port.
    /// For Internet domain sockets, the  socket referred to by sockfd had not previously
    /// been bound to an address and, upon attempting to bind it to an ephemeral port, it
    /// was determined that all port numbers in the ephemeral port range are currently in
    /// use.  See the discussion of /proc/sys/net/ipv4/ip_local_port_range in ip(7).
    AddressInUse,

    /// The file descriptor sockfd does not refer to a socket.
    FileDescriptorNotASocket,

    /// The socket is not of a type that supports the listen() operation.
    OperationNotSupported,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn posixListen(sockfd: i32, backlog: u32) PosixListenError!void {
    const rc = posix.listen(sockfd, backlog);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return,
        posix.EADDRINUSE => return PosixListenError.AddressInUse,
        posix.EBADF => unreachable,
        posix.ENOTSOCK => return PosixListenError.FileDescriptorNotASocket,
        posix.EOPNOTSUPP => return PosixListenError.OperationNotSupported,
        else => return unexpectedErrorPosix(err),
    }
}

pub const PosixAcceptError = error{
    ConnectionAborted,

    /// The per-process limit on the number of open file descriptors has been reached.
    ProcessFdQuotaExceeded,

    /// The system-wide limit on the total number of open files has been reached.
    SystemFdQuotaExceeded,

    /// Not enough free memory.  This often means that the memory allocation  is  limited
    /// by the socket buffer limits, not by the system memory.
    SystemResources,

    /// The file descriptor sockfd does not refer to a socket.
    FileDescriptorNotASocket,

    /// The referenced socket is not of type SOCK_STREAM.
    OperationNotSupported,

    ProtocolFailure,

    /// Firewall rules forbid connection.
    BlockedByFirewall,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn posixAccept(fd: i32, addr: *posix.sockaddr, flags: u32) PosixAcceptError!i32 {
    while (true) {
        var sockaddr_size = u32(@sizeOf(posix.sockaddr));
        const rc = posix.accept4(fd, addr, &sockaddr_size, flags);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return @intCast(i32, rc),
            posix.EINTR => continue,
            else => return unexpectedErrorPosix(err),

            posix.EAGAIN => unreachable, // use posixAsyncAccept for non-blocking
            posix.EBADF => unreachable, // always a race condition
            posix.ECONNABORTED => return PosixAcceptError.ConnectionAborted,
            posix.EFAULT => unreachable,
            posix.EINVAL => unreachable,
            posix.EMFILE => return PosixAcceptError.ProcessFdQuotaExceeded,
            posix.ENFILE => return PosixAcceptError.SystemFdQuotaExceeded,
            posix.ENOBUFS => return PosixAcceptError.SystemResources,
            posix.ENOMEM => return PosixAcceptError.SystemResources,
            posix.ENOTSOCK => return PosixAcceptError.FileDescriptorNotASocket,
            posix.EOPNOTSUPP => return PosixAcceptError.OperationNotSupported,
            posix.EPROTO => return PosixAcceptError.ProtocolFailure,
            posix.EPERM => return PosixAcceptError.BlockedByFirewall,
        }
    }
}

/// Returns -1 if would block.
pub fn posixAsyncAccept(fd: i32, addr: *posix.sockaddr, flags: u32) PosixAcceptError!i32 {
    while (true) {
        var sockaddr_size = u32(@sizeOf(posix.sockaddr));
        const rc = posix.accept4(fd, addr, &sockaddr_size, flags);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return @intCast(i32, rc),
            posix.EINTR => continue,
            else => return unexpectedErrorPosix(err),

            posix.EAGAIN => return -1,
            posix.EBADF => unreachable, // always a race condition
            posix.ECONNABORTED => return PosixAcceptError.ConnectionAborted,
            posix.EFAULT => unreachable,
            posix.EINVAL => unreachable,
            posix.EMFILE => return PosixAcceptError.ProcessFdQuotaExceeded,
            posix.ENFILE => return PosixAcceptError.SystemFdQuotaExceeded,
            posix.ENOBUFS => return PosixAcceptError.SystemResources,
            posix.ENOMEM => return PosixAcceptError.SystemResources,
            posix.ENOTSOCK => return PosixAcceptError.FileDescriptorNotASocket,
            posix.EOPNOTSUPP => return PosixAcceptError.OperationNotSupported,
            posix.EPROTO => return PosixAcceptError.ProtocolFailure,
            posix.EPERM => return PosixAcceptError.BlockedByFirewall,
        }
    }
}

pub const LinuxEpollCreateError = error{
    /// The  per-user   limit   on   the   number   of   epoll   instances   imposed   by
    /// /proc/sys/fs/epoll/max_user_instances  was encountered.  See epoll(7) for further
    /// details.
    /// Or, The per-process limit on the number of open file descriptors has been reached.
    ProcessFdQuotaExceeded,

    /// The system-wide limit on the total number of open files has been reached.
    SystemFdQuotaExceeded,

    /// There was insufficient memory to create the kernel object.
    SystemResources,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn linuxEpollCreate(flags: u32) LinuxEpollCreateError!i32 {
    const rc = posix.epoll_create1(flags);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        else => return unexpectedErrorPosix(err),

        posix.EINVAL => unreachable,
        posix.EMFILE => return LinuxEpollCreateError.ProcessFdQuotaExceeded,
        posix.ENFILE => return LinuxEpollCreateError.SystemFdQuotaExceeded,
        posix.ENOMEM => return LinuxEpollCreateError.SystemResources,
    }
}

pub const LinuxEpollCtlError = error{
    /// op was EPOLL_CTL_ADD, and the supplied file descriptor fd is  already  registered
    /// with this epoll instance.
    FileDescriptorAlreadyPresentInSet,

    /// fd refers to an epoll instance and this EPOLL_CTL_ADD operation would result in a
    /// circular loop of epoll instances monitoring one another.
    OperationCausesCircularLoop,

    /// op was EPOLL_CTL_MOD or EPOLL_CTL_DEL, and fd is not registered with  this  epoll
    /// instance.
    FileDescriptorNotRegistered,

    /// There was insufficient memory to handle the requested op control operation.
    SystemResources,

    /// The  limit  imposed  by /proc/sys/fs/epoll/max_user_watches was encountered while
    /// trying to register (EPOLL_CTL_ADD) a new file descriptor on  an  epoll  instance.
    /// See epoll(7) for further details.
    UserResourceLimitReached,

    /// The target file fd does not support epoll.  This error can occur if fd refers to,
    /// for example, a regular file or a directory.
    FileDescriptorIncompatibleWithEpoll,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn linuxEpollCtl(epfd: i32, op: u32, fd: i32, event: *linux.epoll_event) LinuxEpollCtlError!void {
    const rc = posix.epoll_ctl(epfd, op, fd, event);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return,
        else => return unexpectedErrorPosix(err),

        posix.EBADF => unreachable, // always a race condition if this happens
        posix.EEXIST => return LinuxEpollCtlError.FileDescriptorAlreadyPresentInSet,
        posix.EINVAL => unreachable,
        posix.ELOOP => return LinuxEpollCtlError.OperationCausesCircularLoop,
        posix.ENOENT => return LinuxEpollCtlError.FileDescriptorNotRegistered,
        posix.ENOMEM => return LinuxEpollCtlError.SystemResources,
        posix.ENOSPC => return LinuxEpollCtlError.UserResourceLimitReached,
        posix.EPERM => return LinuxEpollCtlError.FileDescriptorIncompatibleWithEpoll,
    }
}

pub fn linuxEpollWait(epfd: i32, events: []linux.epoll_event, timeout: i32) usize {
    while (true) {
        const rc = posix.epoll_wait(epfd, events.ptr, @intCast(u32, events.len), timeout);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return rc,
            posix.EINTR => continue,
            posix.EBADF => unreachable,
            posix.EFAULT => unreachable,
            posix.EINVAL => unreachable,
            else => unreachable,
        }
    }
}

pub const LinuxEventFdError = error{
    InvalidFlagValue,
    SystemResources,
    ProcessFdQuotaExceeded,
    SystemFdQuotaExceeded,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn linuxEventFd(initval: u32, flags: u32) LinuxEventFdError!i32 {
    const rc = posix.eventfd(initval, flags);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        else => return unexpectedErrorPosix(err),

        posix.EINVAL => return LinuxEventFdError.InvalidFlagValue,
        posix.EMFILE => return LinuxEventFdError.ProcessFdQuotaExceeded,
        posix.ENFILE => return LinuxEventFdError.SystemFdQuotaExceeded,
        posix.ENODEV => return LinuxEventFdError.SystemResources,
        posix.ENOMEM => return LinuxEventFdError.SystemResources,
    }
}

pub const PosixGetSockNameError = error{
    /// Insufficient resources were available in the system to perform the operation.
    SystemResources,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn posixGetSockName(sockfd: i32) PosixGetSockNameError!posix.sockaddr {
    var addr: posix.sockaddr = undefined;
    var addrlen: posix.socklen_t = @sizeOf(posix.sockaddr);
    const rc = posix.getsockname(sockfd, &addr, &addrlen);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return addr,
        else => return unexpectedErrorPosix(err),

        posix.EBADF => unreachable,
        posix.EFAULT => unreachable,
        posix.EINVAL => unreachable,
        posix.ENOTSOCK => unreachable,
        posix.ENOBUFS => return PosixGetSockNameError.SystemResources,
    }
}

pub const PosixConnectError = error{
    /// For UNIX domain sockets, which are identified by pathname: Write permission is denied on  the  socket
    /// file,  or  search  permission  is  denied  for  one of the directories in the path prefix.
    /// or
    /// The user tried to connect to a broadcast address without having the socket broadcast flag enabled  or
    /// the connection request failed because of a local firewall rule.
    PermissionDenied,

    /// Local address is already in use.
    AddressInUse,

    /// (Internet  domain  sockets)  The  socket  referred  to  by sockfd had not previously been bound to an
    /// address and, upon attempting to bind it to an ephemeral port, it was determined that all port numbers
    /// in    the    ephemeral    port    range    are   currently   in   use.    See   the   discussion   of
    /// /proc/sys/net/ipv4/ip_local_port_range in ip(7).
    AddressNotAvailable,

    /// The passed address didn't have the correct address family in its sa_family field.
    AddressFamilyNotSupported,

    /// Insufficient entries in the routing cache.
    SystemResources,

    /// A connect() on a stream socket found no one listening on the remote address.
    ConnectionRefused,

    /// Network is unreachable.
    NetworkUnreachable,

    /// Timeout  while  attempting  connection.   The server may be too busy to accept new connections.  Note
    /// that for IP sockets the timeout may be very long when syncookies are enabled on the server.
    ConnectionTimedOut,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn posixConnect(sockfd: i32, sockaddr: *const posix.sockaddr) PosixConnectError!void {
    while (true) {
        const rc = posix.connect(sockfd, sockaddr, @sizeOf(posix.sockaddr));
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return,
            else => return unexpectedErrorPosix(err),

            posix.EACCES => return PosixConnectError.PermissionDenied,
            posix.EPERM => return PosixConnectError.PermissionDenied,
            posix.EADDRINUSE => return PosixConnectError.AddressInUse,
            posix.EADDRNOTAVAIL => return PosixConnectError.AddressNotAvailable,
            posix.EAFNOSUPPORT => return PosixConnectError.AddressFamilyNotSupported,
            posix.EAGAIN => return PosixConnectError.SystemResources,
            posix.EALREADY => unreachable, // The socket is nonblocking and a previous connection attempt has not yet been completed.
            posix.EBADF => unreachable, // sockfd is not a valid open file descriptor.
            posix.ECONNREFUSED => return PosixConnectError.ConnectionRefused,
            posix.EFAULT => unreachable, // The socket structure address is outside the user's address space.
            posix.EINPROGRESS => unreachable, // The socket is nonblocking and the connection cannot be completed immediately.
            posix.EINTR => continue,
            posix.EISCONN => unreachable, // The socket is already connected.
            posix.ENETUNREACH => return PosixConnectError.NetworkUnreachable,
            posix.ENOTSOCK => unreachable, // The file descriptor sockfd does not refer to a socket.
            posix.EPROTOTYPE => unreachable, // The socket type does not support the requested communications protocol.
            posix.ETIMEDOUT => return PosixConnectError.ConnectionTimedOut,
        }
    }
}

/// Same as posixConnect except it is for blocking socket file descriptors.
/// It expects to receive EINPROGRESS.
pub fn posixConnectAsync(sockfd: i32, sockaddr: *const c_void, len: u32) PosixConnectError!void {
    while (true) {
        const rc = posix.connect(sockfd, sockaddr, len);
        const err = posix.getErrno(rc);
        switch (err) {
            0, posix.EINPROGRESS => return,
            else => return unexpectedErrorPosix(err),

            posix.EACCES => return PosixConnectError.PermissionDenied,
            posix.EPERM => return PosixConnectError.PermissionDenied,
            posix.EADDRINUSE => return PosixConnectError.AddressInUse,
            posix.EADDRNOTAVAIL => return PosixConnectError.AddressNotAvailable,
            posix.EAFNOSUPPORT => return PosixConnectError.AddressFamilyNotSupported,
            posix.EAGAIN => return PosixConnectError.SystemResources,
            posix.EALREADY => unreachable, // The socket is nonblocking and a previous connection attempt has not yet been completed.
            posix.EBADF => unreachable, // sockfd is not a valid open file descriptor.
            posix.ECONNREFUSED => return PosixConnectError.ConnectionRefused,
            posix.EFAULT => unreachable, // The socket structure address is outside the user's address space.
            posix.EINTR => continue,
            posix.EISCONN => unreachable, // The socket is already connected.
            posix.ENETUNREACH => return PosixConnectError.NetworkUnreachable,
            posix.ENOTSOCK => unreachable, // The file descriptor sockfd does not refer to a socket.
            posix.EPROTOTYPE => unreachable, // The socket type does not support the requested communications protocol.
            posix.ETIMEDOUT => return PosixConnectError.ConnectionTimedOut,
        }
    }
}

pub fn posixGetSockOptConnectError(sockfd: i32) PosixConnectError!void {
    var err_code: i32 = undefined;
    var size: u32 = @sizeOf(i32);
    const rc = posix.getsockopt(sockfd, posix.SOL_SOCKET, posix.SO_ERROR, @ptrCast([*]u8, &err_code), &size);
    assert(size == 4);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => switch (err_code) {
            0 => return,
            else => return unexpectedErrorPosix(err),

            posix.EACCES => return PosixConnectError.PermissionDenied,
            posix.EPERM => return PosixConnectError.PermissionDenied,
            posix.EADDRINUSE => return PosixConnectError.AddressInUse,
            posix.EADDRNOTAVAIL => return PosixConnectError.AddressNotAvailable,
            posix.EAFNOSUPPORT => return PosixConnectError.AddressFamilyNotSupported,
            posix.EAGAIN => return PosixConnectError.SystemResources,
            posix.EALREADY => unreachable, // The socket is nonblocking and a previous connection attempt has not yet been completed.
            posix.EBADF => unreachable, // sockfd is not a valid open file descriptor.
            posix.ECONNREFUSED => return PosixConnectError.ConnectionRefused,
            posix.EFAULT => unreachable, // The socket structure address is outside the user's address space.
            posix.EISCONN => unreachable, // The socket is already connected.
            posix.ENETUNREACH => return PosixConnectError.NetworkUnreachable,
            posix.ENOTSOCK => unreachable, // The file descriptor sockfd does not refer to a socket.
            posix.EPROTOTYPE => unreachable, // The socket type does not support the requested communications protocol.
            posix.ETIMEDOUT => return PosixConnectError.ConnectionTimedOut,
        },
        else => return unexpectedErrorPosix(err),
        posix.EBADF => unreachable, // The argument sockfd is not a valid file descriptor.
        posix.EFAULT => unreachable, // The address pointed to by optval or optlen is not in a valid part of the process address space.
        posix.EINVAL => unreachable,
        posix.ENOPROTOOPT => unreachable, // The option is unknown at the level indicated.
        posix.ENOTSOCK => unreachable, // The file descriptor sockfd does not refer to a socket.
    }
}

pub const Thread = struct {
    data: Data,

    pub const use_pthreads = is_posix and builtin.link_libc;

    /// Represents a kernel thread handle.
    /// May be an integer or a pointer depending on the platform.
    /// On Linux and POSIX, this is the same as Id.
    pub const Handle = if (use_pthreads)
        c.pthread_t
    else switch (builtin.os) {
        builtin.Os.linux => i32,
        builtin.Os.windows => windows.HANDLE,
        else => @compileError("Unsupported OS"),
    };

    /// Represents a unique ID per thread.
    /// May be an integer or pointer depending on the platform.
    /// On Linux and POSIX, this is the same as Handle.
    pub const Id = switch (builtin.os) {
        builtin.Os.windows => windows.DWORD,
        else => Handle,
    };

    pub const Data = if (use_pthreads)
        struct {
            handle: Thread.Handle,
            mmap_addr: usize,
            mmap_len: usize,
        }
    else switch (builtin.os) {
        builtin.Os.linux => struct {
            handle: Thread.Handle,
            mmap_addr: usize,
            mmap_len: usize,
        },
        builtin.Os.windows => struct {
            handle: Thread.Handle,
            alloc_start: *c_void,
            heap_handle: windows.HANDLE,
        },
        else => @compileError("Unsupported OS"),
    };

    /// Returns the ID of the calling thread.
    /// Makes a syscall every time the function is called.
    /// On Linux and POSIX, this Id is the same as a Handle.
    pub fn getCurrentId() Id {
        if (use_pthreads) {
            return c.pthread_self();
        } else
            return switch (builtin.os) {
            builtin.Os.linux => linux.gettid(),
            builtin.Os.windows => windows.GetCurrentThreadId(),
            else => @compileError("Unsupported OS"),
        };
    }

    /// Returns the handle of this thread.
    /// On Linux and POSIX, this is the same as Id.
    /// On Linux, it is possible that the thread spawned with `spawnThread`
    /// finishes executing entirely before the clone syscall completes. In this
    /// case, this function will return 0 rather than the no-longer-existing thread's
    /// pid.
    pub fn handle(self: Thread) Handle {
        return self.data.handle;
    }

    pub fn wait(self: *const Thread) void {
        if (use_pthreads) {
            const err = c.pthread_join(self.data.handle, null);
            switch (err) {
                0 => {},
                posix.EINVAL => unreachable,
                posix.ESRCH => unreachable,
                posix.EDEADLK => unreachable,
                else => unreachable,
            }
            assert(posix.munmap(self.data.mmap_addr, self.data.mmap_len) == 0);
        } else switch (builtin.os) {
            builtin.Os.linux => {
                while (true) {
                    const pid_value = @atomicLoad(i32, &self.data.handle, .SeqCst);
                    if (pid_value == 0) break;
                    const rc = linux.futex_wait(&self.data.handle, linux.FUTEX_WAIT, pid_value, null);
                    switch (linux.getErrno(rc)) {
                        0 => continue,
                        posix.EINTR => continue,
                        posix.EAGAIN => continue,
                        else => unreachable,
                    }
                }
                assert(posix.munmap(self.data.mmap_addr, self.data.mmap_len) == 0);
            },
            builtin.Os.windows => {
                assert(windows.WaitForSingleObject(self.data.handle, windows.INFINITE) == windows.WAIT_OBJECT_0);
                assert(windows.CloseHandle(self.data.handle) != 0);
                assert(windows.HeapFree(self.data.heap_handle, 0, self.data.alloc_start) != 0);
            },
            else => @compileError("Unsupported OS"),
        }
    }
};

pub const SpawnThreadError = error{
    /// A system-imposed limit on the number of threads was encountered.
    /// There are a number of limits that may trigger this error:
    /// *  the  RLIMIT_NPROC soft resource limit (set via setrlimit(2)),
    ///    which limits the number of processes and threads for  a  real
    ///    user ID, was reached;
    /// *  the kernel's system-wide limit on the number of processes and
    ///    threads,  /proc/sys/kernel/threads-max,  was   reached   (see
    ///    proc(5));
    /// *  the  maximum  number  of  PIDs, /proc/sys/kernel/pid_max, was
    ///    reached (see proc(5)); or
    /// *  the PID limit (pids.max) imposed by the cgroup "process  num‐
    ///    ber" (PIDs) controller was reached.
    ThreadQuotaExceeded,

    /// The kernel cannot allocate sufficient memory to allocate a task structure
    /// for the child, or to copy those parts of the caller's context that need to
    /// be copied.
    SystemResources,

    /// Not enough userland memory to spawn the thread.
    OutOfMemory,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub var linux_tls_phdr: ?*std.elf.Phdr = null;
pub var linux_tls_img_src: [*]const u8 = undefined; // defined if linux_tls_phdr is

/// caller must call wait on the returned thread
/// fn startFn(@typeOf(context)) T
/// where T is u8, noreturn, void, or !void
/// caller must call wait on the returned thread
pub fn spawnThread(context: var, comptime startFn: var) SpawnThreadError!*Thread {
    if (builtin.single_threaded) @compileError("cannot spawn thread when building in single-threaded mode");
    // TODO compile-time call graph analysis to determine stack upper bound
    // https://github.com/ziglang/zig/issues/157
    const default_stack_size = 8 * 1024 * 1024;

    const Context = @typeOf(context);
    comptime assert(@ArgType(@typeOf(startFn), 0) == Context);

    if (builtin.os == builtin.Os.windows) {
        const WinThread = struct {
            const OuterContext = struct {
                thread: Thread,
                inner: Context,
            };
            extern fn threadMain(raw_arg: windows.LPVOID) windows.DWORD {
                const arg = if (@sizeOf(Context) == 0) {} else @ptrCast(*Context, @alignCast(@alignOf(Context), raw_arg)).*;
                switch (@typeId(@typeOf(startFn).ReturnType)) {
                    builtin.TypeId.Int => {
                        return startFn(arg);
                    },
                    builtin.TypeId.Void => {
                        startFn(arg);
                        return 0;
                    },
                    else => @compileError("expected return type of startFn to be 'u8', 'noreturn', 'void', or '!void'"),
                }
            }
        };

        const heap_handle = windows.GetProcessHeap() orelse return SpawnThreadError.OutOfMemory;
        const byte_count = @alignOf(WinThread.OuterContext) + @sizeOf(WinThread.OuterContext);
        const bytes_ptr = windows.HeapAlloc(heap_handle, 0, byte_count) orelse return SpawnThreadError.OutOfMemory;
        errdefer assert(windows.HeapFree(heap_handle, 0, bytes_ptr) != 0);
        const bytes = @ptrCast([*]u8, bytes_ptr)[0..byte_count];
        const outer_context = std.heap.FixedBufferAllocator.init(bytes).allocator.create(WinThread.OuterContext) catch unreachable;
        outer_context.* = WinThread.OuterContext{
            .thread = Thread{
                .data = Thread.Data{
                    .heap_handle = heap_handle,
                    .alloc_start = bytes_ptr,
                    .handle = undefined,
                },
            },
            .inner = context,
        };

        const parameter = if (@sizeOf(Context) == 0) null else @ptrCast(*c_void, &outer_context.inner);
        outer_context.thread.data.handle = windows.CreateThread(null, default_stack_size, WinThread.threadMain, parameter, 0, null) orelse {
            const err = windows.GetLastError();
            return switch (err) {
                else => os.unexpectedErrorWindows(err),
            };
        };
        return &outer_context.thread;
    }

    const MainFuncs = struct {
        extern fn linuxThreadMain(ctx_addr: usize) u8 {
            const arg = if (@sizeOf(Context) == 0) {} else @intToPtr(*const Context, ctx_addr).*;

            switch (@typeId(@typeOf(startFn).ReturnType)) {
                builtin.TypeId.Int => {
                    return startFn(arg);
                },
                builtin.TypeId.Void => {
                    startFn(arg);
                    return 0;
                },
                else => @compileError("expected return type of startFn to be 'u8', 'noreturn', 'void', or '!void'"),
            }
        }
        extern fn posixThreadMain(ctx: ?*c_void) ?*c_void {
            if (@sizeOf(Context) == 0) {
                _ = startFn({});
                return null;
            } else {
                _ = startFn(@ptrCast(*const Context, @alignCast(@alignOf(Context), ctx)).*);
                return null;
            }
        }
    };

    const MAP_GROWSDOWN = if (builtin.os == builtin.Os.linux) linux.MAP_GROWSDOWN else 0;

    var stack_end_offset: usize = undefined;
    var thread_start_offset: usize = undefined;
    var context_start_offset: usize = undefined;
    var tls_start_offset: usize = undefined;
    const mmap_len = blk: {
        // First in memory will be the stack, which grows downwards.
        var l: usize = mem.alignForward(default_stack_size, os.page_size);
        stack_end_offset = l;
        // Above the stack, so that it can be in the same mmap call, put the Thread object.
        l = mem.alignForward(l, @alignOf(Thread));
        thread_start_offset = l;
        l += @sizeOf(Thread);
        // Next, the Context object.
        if (@sizeOf(Context) != 0) {
            l = mem.alignForward(l, @alignOf(Context));
            context_start_offset = l;
            l += @sizeOf(Context);
        }
        // Finally, the Thread Local Storage, if any.
        if (!Thread.use_pthreads) {
            if (linux_tls_phdr) |tls_phdr| {
                l = mem.alignForward(l, tls_phdr.p_align);
                tls_start_offset = l;
                l += tls_phdr.p_memsz;
                // the fs register address
                l += @sizeOf(usize);
            }
        }
        break :blk l;
    };
    const mmap_addr = posix.mmap(null, mmap_len, posix.PROT_READ | posix.PROT_WRITE, posix.MAP_PRIVATE | posix.MAP_ANONYMOUS | MAP_GROWSDOWN, -1, 0);
    if (mmap_addr == posix.MAP_FAILED) return error.OutOfMemory;
    errdefer assert(posix.munmap(mmap_addr, mmap_len) == 0);

    const thread_ptr = @alignCast(@alignOf(Thread), @intToPtr(*Thread, mmap_addr + thread_start_offset));
    thread_ptr.data.mmap_addr = mmap_addr;
    thread_ptr.data.mmap_len = mmap_len;

    var arg: usize = undefined;
    if (@sizeOf(Context) != 0) {
        arg = mmap_addr + context_start_offset;
        const context_ptr = @alignCast(@alignOf(Context), @intToPtr(*Context, arg));
        context_ptr.* = context;
    }

    if (Thread.use_pthreads) {
        // use pthreads
        var attr: c.pthread_attr_t = undefined;
        if (c.pthread_attr_init(&attr) != 0) return SpawnThreadError.SystemResources;
        defer assert(c.pthread_attr_destroy(&attr) == 0);

        assert(c.pthread_attr_setstack(&attr, @intToPtr(*c_void, mmap_addr), stack_end_offset) == 0);

        const err = c.pthread_create(&thread_ptr.data.handle, &attr, MainFuncs.posixThreadMain, @intToPtr(*c_void, arg));
        switch (err) {
            0 => return thread_ptr,
            posix.EAGAIN => return SpawnThreadError.SystemResources,
            posix.EPERM => unreachable,
            posix.EINVAL => unreachable,
            else => return unexpectedErrorPosix(@intCast(usize, err)),
        }
    } else if (builtin.os == builtin.Os.linux) {
        var flags: u32 = posix.CLONE_VM | posix.CLONE_FS | posix.CLONE_FILES | posix.CLONE_SIGHAND |
            posix.CLONE_THREAD | posix.CLONE_SYSVSEM | posix.CLONE_PARENT_SETTID | posix.CLONE_CHILD_CLEARTID |
            posix.CLONE_DETACHED;
        var newtls: usize = undefined;
        if (linux_tls_phdr) |tls_phdr| {
            @memcpy(@intToPtr([*]u8, mmap_addr + tls_start_offset), linux_tls_img_src, tls_phdr.p_filesz);
            newtls = mmap_addr + mmap_len - @sizeOf(usize);
            @intToPtr(*usize, newtls).* = newtls;
            flags |= posix.CLONE_SETTLS;
        }
        const rc = posix.clone(MainFuncs.linuxThreadMain, mmap_addr + stack_end_offset, flags, arg, &thread_ptr.data.handle, newtls, &thread_ptr.data.handle);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return thread_ptr,
            posix.EAGAIN => return SpawnThreadError.ThreadQuotaExceeded,
            posix.EINVAL => unreachable,
            posix.ENOMEM => return SpawnThreadError.SystemResources,
            posix.ENOSPC => unreachable,
            posix.EPERM => unreachable,
            posix.EUSERS => unreachable,
            else => return unexpectedErrorPosix(err),
        }
    } else {
        @compileError("Unsupported OS");
    }
}

pub fn posixWait(pid: i32) i32 {
    var status: i32 = undefined;
    while (true) {
        const err = posix.getErrno(posix.waitpid(pid, &status, 0));
        switch (err) {
            0 => return status,
            posix.EINTR => continue,
            posix.ECHILD => unreachable, // The process specified does not exist. It would be a race condition to handle this error.
            posix.EINVAL => unreachable, // The options argument was invalid
            else => unreachable,
        }
    }
}

pub fn posixFStat(fd: i32) !posix.Stat {
    var stat: posix.Stat = undefined;
    const err = posix.getErrno(posix.fstat(fd, &stat));
    if (err > 0) {
        return switch (err) {
            // We do not make this an error code because if you get EBADF it's always a bug,
            // since the fd could have been reused.
            posix.EBADF => unreachable,
            posix.ENOMEM => error.SystemResources,
            else => os.unexpectedErrorPosix(err),
        };
    }

    return stat;
}

pub const CpuCountError = error{
    OutOfMemory,
    PermissionDenied,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn cpuCount(fallback_allocator: *mem.Allocator) CpuCountError!usize {
    switch (builtin.os) {
        builtin.Os.macosx, builtin.Os.freebsd, builtin.Os.netbsd => {
            var count: c_int = undefined;
            var count_len: usize = @sizeOf(c_int);
            const rc = posix.sysctlbyname(switch (builtin.os) {
                builtin.Os.macosx => c"hw.logicalcpu",
                else => c"hw.ncpu",
            }, @ptrCast(*c_void, &count), &count_len, null, 0);
            const err = posix.getErrno(rc);
            switch (err) {
                0 => return @intCast(usize, count),
                posix.EFAULT => unreachable,
                posix.EINVAL => unreachable,
                posix.ENOMEM => return CpuCountError.OutOfMemory,
                posix.ENOTDIR => unreachable,
                posix.EISDIR => unreachable,
                posix.ENOENT => unreachable,
                posix.EPERM => unreachable,
                else => return os.unexpectedErrorPosix(err),
            }
        },
        builtin.Os.linux => {
            const usize_count = 16;
            const allocator = std.heap.stackFallback(usize_count * @sizeOf(usize), fallback_allocator).get();

            var set = try allocator.alloc(usize, usize_count);
            defer allocator.free(set);

            while (true) {
                const rc = posix.sched_getaffinity(0, set);
                const err = posix.getErrno(rc);
                switch (err) {
                    0 => {
                        if (rc < set.len * @sizeOf(usize)) {
                            const result = set[0 .. rc / @sizeOf(usize)];
                            var sum: usize = 0;
                            for (result) |x| {
                                sum += @popCount(x);
                            }
                            return sum;
                        } else {
                            set = try allocator.realloc(set, set.len * 2);
                            continue;
                        }
                    },
                    posix.EFAULT => unreachable,
                    posix.EINVAL => unreachable,
                    posix.EPERM => return CpuCountError.PermissionDenied,
                    posix.ESRCH => unreachable,
                    else => return os.unexpectedErrorPosix(err),
                }
            }
        },
        builtin.Os.windows => {
            var system_info: windows.SYSTEM_INFO = undefined;
            windows.GetSystemInfo(&system_info);
            return @intCast(usize, system_info.dwNumberOfProcessors);
        },
        else => @compileError("unsupported OS"),
    }
}

pub const BsdKQueueError = error{
    /// The per-process limit on the number of open file descriptors has been reached.
    ProcessFdQuotaExceeded,

    /// The system-wide limit on the total number of open files has been reached.
    SystemFdQuotaExceeded,

    /// See https://github.com/ziglang/zig/issues/1396
    Unexpected,
};

pub fn bsdKQueue() BsdKQueueError!i32 {
    const rc = posix.kqueue();
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        posix.EMFILE => return BsdKQueueError.ProcessFdQuotaExceeded,
        posix.ENFILE => return BsdKQueueError.SystemFdQuotaExceeded,
        else => return unexpectedErrorPosix(err),
    }
}

pub const BsdKEventError = error{
    /// The process does not have permission to register a filter.
    AccessDenied,

    /// The event could not be found to be modified or deleted.
    EventNotFound,

    /// No memory was available to register the event.
    SystemResources,

    /// The specified process to attach to does not exist.
    ProcessNotFound,
};

pub fn bsdKEvent(
    kq: i32,
    changelist: []const posix.Kevent,
    eventlist: []posix.Kevent,
    timeout: ?*const posix.timespec,
) BsdKEventError!usize {
    while (true) {
        const rc = posix.kevent(kq, changelist, eventlist, timeout);
        const err = posix.getErrno(rc);
        switch (err) {
            0 => return rc,
            posix.EACCES => return BsdKEventError.AccessDenied,
            posix.EFAULT => unreachable,
            posix.EBADF => unreachable,
            posix.EINTR => continue,
            posix.EINVAL => unreachable,
            posix.ENOENT => return BsdKEventError.EventNotFound,
            posix.ENOMEM => return BsdKEventError.SystemResources,
            posix.ESRCH => return BsdKEventError.ProcessNotFound,
            else => unreachable,
        }
    }
}

pub fn linuxINotifyInit1(flags: u32) !i32 {
    const rc = linux.inotify_init1(flags);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        posix.EINVAL => unreachable,
        posix.EMFILE => return error.ProcessFdQuotaExceeded,
        posix.ENFILE => return error.SystemFdQuotaExceeded,
        posix.ENOMEM => return error.SystemResources,
        else => return unexpectedErrorPosix(err),
    }
}

pub fn linuxINotifyAddWatchC(inotify_fd: i32, pathname: [*]const u8, mask: u32) !i32 {
    const rc = linux.inotify_add_watch(inotify_fd, pathname, mask);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return @intCast(i32, rc),
        posix.EACCES => return error.AccessDenied,
        posix.EBADF => unreachable,
        posix.EFAULT => unreachable,
        posix.EINVAL => unreachable,
        posix.ENAMETOOLONG => return error.NameTooLong,
        posix.ENOENT => return error.FileNotFound,
        posix.ENOMEM => return error.SystemResources,
        posix.ENOSPC => return error.UserResourceLimitReached,
        else => return unexpectedErrorPosix(err),
    }
}

pub fn linuxINotifyRmWatch(inotify_fd: i32, wd: i32) !void {
    const rc = linux.inotify_rm_watch(inotify_fd, wd);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return rc,
        posix.EBADF => unreachable,
        posix.EINVAL => unreachable,
        else => unreachable,
    }
}

pub const MProtectError = error{
    AccessDenied,
    OutOfMemory,
    Unexpected,
};

/// address and length must be page-aligned
pub fn posixMProtect(address: usize, length: usize, protection: u32) MProtectError!void {
    const negative_page_size = @bitCast(usize, -isize(page_size));
    const aligned_address = address & negative_page_size;
    const aligned_end = (address + length + page_size - 1) & negative_page_size;
    assert(address == aligned_address);
    assert(length == aligned_end - aligned_address);
    const rc = posix.mprotect(address, length, protection);
    const err = posix.getErrno(rc);
    switch (err) {
        0 => return,
        posix.EINVAL => unreachable,
        posix.EACCES => return error.AccessDenied,
        posix.ENOMEM => return error.OutOfMemory,
        else => return unexpectedErrorPosix(err),
    }
}
