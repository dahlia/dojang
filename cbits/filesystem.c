#ifndef _WIN32

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <sys/stat.h>

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
