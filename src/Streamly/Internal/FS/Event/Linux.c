/*
 * Linux helpers of Streamly.Internal.FS.Event.Linux.
 *
 * The statx system call is made directly, glibc provides a wrapper only
 * since 2.28 and musl since 1.2.5. <linux/stat.h> is used instead of
 * <sys/stat.h>, the struct statx of glibc conflicts with it.
 */

#include <fcntl.h>
#include <linux/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

/*
 * Whether the path is the root of a mount, for example, of a bind mount.
 * Returns 1 if it is, 0 if it is not, and -1 if it cannot be determined:
 * the kernel is older than 5.8, the headers do not define statx, or statx
 * fails.
 */
int streamly_fsevents_is_mount_root(const char *path, int follow)
{
#if defined(SYS_statx) && defined(STATX_ATTR_MOUNT_ROOT)
    struct statx stx;
    int flags = follow ? 0 : AT_SYMLINK_NOFOLLOW;

    if (syscall(SYS_statx, AT_FDCWD, path, flags, 0, &stx) != 0) {
        return -1;
    }
    if (!(stx.stx_attributes_mask & STATX_ATTR_MOUNT_ROOT)) {
        return -1;
    }
    return (stx.stx_attributes & STATX_ATTR_MOUNT_ROOT) != 0;
#else
    (void)path;
    (void)follow;
    return -1;
#endif
}
