#ifndef _WIN32

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/syscall.h>
#endif

void *dojang_fdopendir(int fd)
{
    return fdopendir(fd);
}

/*
 * Return 1 after copying one name, 0 at end of stream, or a negated errno.
 */
int dojang_readdir_name(void *stream, char *buffer, size_t buffer_size)
{
    struct dirent *entry;
    size_t length;

    errno = 0;
    entry = readdir((DIR *) stream);
    if (entry == NULL) {
        return errno == 0 ? 0 : -errno;
    }

    length = strlen(entry->d_name);
    if (length + 1 > buffer_size) {
        return -ENAMETOOLONG;
    }
    memcpy(buffer, entry->d_name, length + 1);
    return 1;
}

void dojang_closedir(void *stream)
{
    (void) closedir((DIR *) stream);
}

#ifdef __linux__
int dojang_renameat2(
    int old_directory,
    const char *old_path,
    int new_directory,
    const char *new_path,
    unsigned int flags
)
{
#ifdef SYS_renameat2
    return (int) syscall(
        SYS_renameat2,
        old_directory,
        old_path,
        new_directory,
        new_path,
        flags
    );
#else
    errno = ENOSYS;
    return -1;
#endif
}
#endif

/*
 * Return 1 for a directory, 2 for a symbolic link, 3 for another entry, or a
 * negated errno.  The Haskell decoder in Dojang.MonadFileSystem mirrors these
 * constants.
 */
int dojang_file_type_at(int fd, const char *name)
{
    struct stat status;

    if (fstatat(fd, name, &status, AT_SYMLINK_NOFOLLOW) != 0) {
        return -errno;
    }
    if (S_ISLNK(status.st_mode)) {
        return 2;
    }
    if (S_ISDIR(status.st_mode)) {
        return 1;
    }
    return 3;
}

static int dojang_fsync(int fd)
{
    int result;

    do {
        result = fsync(fd);
    } while (result != 0 && errno == EINTR);
    return result;
}

static void dojang_remove_created_file_at(int fd, const char *name)
{
    (void) unlinkat(fd, name, 0);
    (void) dojang_fsync(fd);
}

/*
 * Make a newly created file and its directory entry durable.  Remove the
 * entry on failure so callers never accept a recovery artifact that has not
 * reached stable storage.
 */
static int dojang_finish_created_file_at(
    int fd,
    const char *name,
    int created
)
{
    int saved_errno;

    if (dojang_fsync(created) != 0) {
        saved_errno = errno;
        (void) close(created);
        dojang_remove_created_file_at(fd, name);
        return -saved_errno;
    }
    if (close(created) != 0) {
        saved_errno = errno;
        dojang_remove_created_file_at(fd, name);
        return -saved_errno;
    }
    if (dojang_fsync(fd) != 0) {
        saved_errno = errno;
        dojang_remove_created_file_at(fd, name);
        return -saved_errno;
    }
    return 1;
}

/*
 * Create and durably publish one empty regular file relative to an already
 * pinned directory.  Return 1 on success or a negated errno.
 */
int dojang_create_empty_file_at(int fd, const char *name)
{
    int created;

    created = openat(
        fd,
        name,
        O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC,
        0666
    );
    if (created == -1) {
        return -errno;
    }
    return dojang_finish_created_file_at(fd, name, created);
}

/*
 * Create and durably publish one owner-only regular file with complete
 * contents relative to an already pinned directory.  Return 1 on success or
 * a negated errno.
 */
int dojang_create_private_file_at(
    int fd,
    const char *name,
    const unsigned char *contents,
    size_t length
)
{
    int created;
    int saved_errno;
    size_t offset;

    created = openat(
        fd,
        name,
        O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC,
        0600
    );
    if (created == -1) {
        return -errno;
    }
    if (fchmod(created, 0600) != 0) {
        saved_errno = errno;
        (void) close(created);
        dojang_remove_created_file_at(fd, name);
        return -saved_errno;
    }
    offset = 0;
    while (offset < length) {
        size_t remaining = length - offset;
        size_t chunk = remaining > 1048576 ? 1048576 : remaining;
        ssize_t written = write(created, contents + offset, chunk);
        if (written < 0) {
            if (errno == EINTR) {
                continue;
            }
            saved_errno = errno;
            (void) close(created);
            dojang_remove_created_file_at(fd, name);
            return -saved_errno;
        }
        if (written == 0) {
            (void) close(created);
            dojang_remove_created_file_at(fd, name);
            return -EIO;
        }
        offset += (size_t) written;
    }
    return dojang_finish_created_file_at(fd, name, created);
}

/*
 * Remove one non-directory entry relative to an already pinned directory.
 * Return 1 on success or a negated errno.
 */
int dojang_remove_file_at(int fd, const char *name)
{
    if (unlinkat(fd, name, 0) != 0) {
        return -errno;
    }
    return 1;
}

#endif
