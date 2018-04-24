#ifndef MAX_PATH
#if defined _MAX_PATH
#define MAX_PATH _MAX_PATH
#elif defined PATH_MAX
#define MAX_PATH PATH_MAX
#else
#error "No suitable MAX_PATH surrogate"
#endif
#endif

void path_normalize(char *path) {
    char *ptr;
    for (ptr = path; *ptr; ptr++) {
        if (*ptr == '\\') {
            *ptr = '/';
        }
    }
    if (ptr != path && ptr[-1] == '/') {
        ptr[-1] = 0;
    }
}

void path_copy(char path[MAX_PATH], const char *src) {
    strncpy(path, src, MAX_PATH);
    path[MAX_PATH - 1] = 0;
    path_normalize(path);
}

void path_join(char path[MAX_PATH], const char *src) {
    char *ptr = path + strlen(path);
    if (ptr != path && ptr[-1] == '/') {
        ptr--;
    }
    if (*src == '/') {
        src++;
    }
    snprintf(ptr, path + MAX_PATH - ptr, "/%s", src);
}

char *path_file(char path[MAX_PATH]) {
    path_normalize(path);
    for (char *ptr = path + strlen(path); ptr != path; ptr--) {
        if (ptr[-1] == '/') {
            return ptr;
        }
    }
    return path;
}

char *path_ext(char path[MAX_PATH]) {
    for (char *ptr = path + strlen(path); ptr != path; ptr--) {
        if (ptr[-1] == '.') {
            return ptr;
        }
    }
    return path;
}

typedef struct DirListIter {
    bool valid;
    bool error;

    char base[MAX_PATH];
    char name[MAX_PATH];
    size_t size;
    bool is_dir;

    void *handle;
} DirListIter;

bool dir_excluded(DirListIter *iter) {
    return iter->valid && (strcmp(iter->name, ".") == 0 || strcmp(iter->name, "..") == 0);
}

#ifdef _MSC_VER
#include "os_win32.c"
#define strdup _strdup
#else
#include "os_unix.c"
#endif

bool dir_list_subdir(DirListIter *iter) {
    if (!iter->valid || !iter->is_dir) {
        return false;
    }
    DirListIter subdir_iter;
    path_join(iter->base, iter->name);
    dir_list(&subdir_iter, iter->base);
    dir_list_free(iter);
    *iter = subdir_iter;
    return true;
}

const char **dir_list_buf(const char *filespec) {
    const char **buf = NULL;
    DirListIter iter;
    for (dir_list(&iter, filespec); iter.valid; dir_list_next(&iter)) {
        const char *name = strdup(iter.name);
        buf_push(buf, name);
    }
    return buf;
}
