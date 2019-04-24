const std = @import("../../std.zig");
const nightingale = std.os.nightingale;
const err = @import("./errno.h");

pub const SYS_INVALID = 0;
pub const SYS_DEBUGPRINT = 1;
pub const SYS_EXIT = 2;
pub const SYS_OPEN = 3;
pub const SYS_READ = 4;
pub const SYS_WRITE = 5;
pub const SYS_FORK = 6;
pub const SYS_TOP = 7;
pub const SYS_GETPID = 8;
pub const SYS_GETTID = 9;
pub const SYS_EXECVE = 10;
pub const SYS_WAIT4 = 11;
pub const SYS_SOCKET = 12;
pub const SYS_BIND0 = 13;
pub const SYS_CONNECT0 = 14;
pub const SYS_STRACE = 15;
pub const SYS_BIND = 16;
pub const SYS_CONNECT = 17;
pub const SYS_SEND = 18;
pub const SYS_SENDTO = 19;
pub const SYS_RECV = 20;
pub const SYS_RECVFROM = 21;
pub const SYS_WAITPID = 22;
pub const SYS_DUP2 = 23;
pub const SYS_UNAME = 24;
pub const SYS_YIELD = 25;
pub const SYS_SEEK = 26;
pub const SYS_POLL = 27;
pub const SYS_MMAP = 28;
pub const SYS_MUNMAP = 29;
pub const SYS_HEAPDBG = 30;
pub const SYS_SETPGID = 31;
pub const SYS_EXIT_GROUP = 32;
pub const SYS_CLONE0 = 33;

pub fn syscall0(number: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall1(number: usize, arg1: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall2(number: usize, arg1: usize, arg2: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1),
          [_] "{si}" (arg2)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall3(number: usize, arg1: usize, arg2: usize, arg3: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1),
          [_] "{si}" (arg2),
          [_] "{dx}" (arg3)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall4(number: usize, arg1: usize, arg2: usize, arg3: usize, arg4: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1),
          [_] "{si}" (arg2),
          [_] "{dx}" (arg3),
          [_] "{cx}" (arg4)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall5(number: usize, arg1: usize, arg2: usize, arg3: usize, arg4: usize, arg5: usize) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ mov %[arg5] %%r8
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1),
          [_] "{si}" (arg2),
          [_] "{dx}" (arg3),
          [_] "{cx}" (arg4),
          [arg5] "rm" (arg5)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

pub fn syscall6(
    number: usize,
    arg1: usize,
    arg2: usize,
    arg3: usize,
    arg4: usize,
    arg5: usize,
    arg6: usize,
) usize {
    var result: usize = undefined;
    var is_error: bool = undefined;

    asm volatile (
        \\ mov %[arg5] %%r8
        \\ mov %[arg6] %%r9
        \\ int $0x80
        : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
        : [_] "0" (number),
          [_] "{dx}" (arg1),
          [_] "{si}" (arg2),
          [_] "{dx}" (arg3),
          [_] "{cx}" (arg4),
          [arg5] "rm" (arg5),
          [arg6] "rm" (arg6)
    );

    if (is_error) {
        return -result;
    }
    return result;
}

// pub fn syscall0(number: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall1(number: usize, arg1: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall2(number: usize, arg1: usize, arg2: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1),
//           [_] "{si}" (arg2)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall3(number: usize, arg1: usize, arg2: usize, arg3: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1),
//           [_] "{si}" (arg2),
//           [_] "{dx}" (arg3)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall4(number: usize, arg1: usize, arg2: usize, arg3: usize, arg4: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1),
//           [_] "{si}" (arg2),
//           [_] "{dx}" (arg3),
//           [_] "{cx}" (arg4)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall5(number: usize, arg1: usize, arg2: usize, arg3: usize, arg4: usize, arg5: usize) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ mov %[arg5] %%r8
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1),
//           [_] "{si}" (arg2),
//           [_] "{dx}" (arg3),
//           [_] "{cx}" (arg4),
//           [arg5] "rm" (arg5)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
// pub fn syscall6(
//     number: usize,
//     arg1: usize,
//     arg2: usize,
//     arg3: usize,
//     arg4: usize,
//     arg5: usize,
//     arg6: usize,
// ) !usize {
//     var result: usize = undefined;
//     var is_error: bool = undefined;
// 
//     asm volatile (
//         \\ mov %[arg5] %%r8
//         \\ mov %[arg6] %%r9
//         \\ int $0x80
//         : [_] "={ax}" (result), [_] "={@ccc}" (is_error)
//         : [_] "0" (number),
//           [_] "{dx}" (arg1),
//           [_] "{si}" (arg2),
//           [_] "{dx}" (arg3),
//           [_] "{cx}" (arg4),
//           [arg5] "rm" (arg5),
//           [arg6] "rm" (arg6)
//     );
// 
//     if (is_error) {
//         return err.ngErrnoToError(result);
//     }
//     return result;
// }
// 
