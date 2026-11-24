/*
 * Linux helpers of Streamly.Internal.FS.Event.Linux.
 *
 * The statx system call is made directly, glibc provides a wrapper only
 * since 2.28 and musl since 1.2.5. <linux/stat.h> is used instead of
 * <sys/stat.h>, the struct statx of glibc conflicts with it.
 *
 * _GNU_SOURCE is required for name_to_handle_at.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <fcntl.h>
#include <linux/stat.h>
#include <string.h>
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

/*
 * Copy the file handle of the file of an fd, from name_to_handle_at, to buf:
 * the handle type followed by the handle bytes. Returns the number of bytes
 * copied, or -1 if the file system does not support file handles, or the
 * handle does not fit in size bytes.
 */
int streamly_fsevents_file_handle(int fd, unsigned char *buf, int size)
{
#if defined(MAX_HANDLE_SZ) && defined(AT_EMPTY_PATH)
    union {
        struct file_handle fh;
        unsigned char bytes[sizeof(struct file_handle) + MAX_HANDLE_SZ];
    } h;
    int mount_id;
    int len;

    h.fh.handle_bytes = MAX_HANDLE_SZ;
    if (name_to_handle_at(fd, "", &h.fh, &mount_id, AT_EMPTY_PATH) != 0) {
        return -1;
    }
    len = (int)sizeof(h.fh.handle_type) + (int)h.fh.handle_bytes;
    if (len > size) {
        return -1;
    }
    memcpy(buf, &h.fh.handle_type, sizeof(h.fh.handle_type));
    memcpy(buf + sizeof(h.fh.handle_type), h.fh.f_handle, h.fh.handle_bytes);
    return len;
#else
    (void)fd;
    (void)buf;
    (void)size;
    return -1;
#endif
}
