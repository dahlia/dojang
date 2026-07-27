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

#ifdef __linux__
#include <sys/syscall.h>
#include <unistd.h>
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

#endif
