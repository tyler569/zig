
pub const SUCCESS = 0;
pub const EINVAL = 1;
pub const EAGAIN = 2;
pub const EWOULDBLOCK = EAGAIN;
pub const ENOEXEC = 3;
pub const ENOENT = 4;
pub const ENOAFSUPPORT = 5;
pub const ECHILD = 6;
pub const EPERM = 7;
pub const EFAULT = 8;
pub const EBADF = 9;

pub const EINTR = 1000;
pub const EDESTADDRREQ = 1001;
pub const EDQUOT = 1002;
pub const EFBIG = 1003;
pub const EIO = 1004;
pub const ENOSPC = 1005;
pub const EPIPE = 1006;
pub const EISDIR = 1007;
pub const ENOBUFS = 1008;
pub const ENOMEM = 1009;

pub const NgError = error {
    EINVAL,
    EAGAIN,
    ENOEXEC,
    ENOENT,
    ENOAFSUPPRT,
    ECHILD,
    EPERM,
    EFAULT,
    EBADF,
};

pub fn ngErrnoToError(ng_errno: usize) NgError {
    return switch (ng_errno) {
        EINVAL => error.EINVAL,
        EAGAIN => error.EAGAIN,
        ENOEXEC => error.ENOEXEC,
        ENOENT => error.ENOENT,
        ENOAFSUPPORT => error.ENOAFSUPPORT,
        ECHILD => error.ECHILD,
        EPERM => error.EPERM,
        EFAULT => error.EFAULT,
        EBADF => error.EBADF,
        else => error.EINVAL,
    };
}

pub fn ngErrorToErrno(ng_errno: NgError) usize {
    return switch (ng_errno) {
        error.EINVAL => EINVAL,
        error.EAGAIN => EAGAIN,
        error.ENOEXEC => ENOEXEC,
        error.ENOENT => ENOENT,
        error.ENOAFSUPPORT => ENOAFSUPPORT,
        error.ECHILD => ECHILD,
        error.EPERM => EPERM,
        error.EFAULT => EFAULT,
        error.EBADF => EBADF,
        error.EINVAL => EINVAL,
    };
}
