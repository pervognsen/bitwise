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


// Command line flag parsing

typedef enum FlagKind {
    FLAG_BOOL,
    FLAG_STR,
    FLAG_ENUM,
} FlagKind;

typedef struct FlagDef {
    FlagKind kind;
    const char *name;
    const char *help;
    const char **options;
    const char *arg_name;
    int num_options;
    struct {
        int *i;
        bool *b;
        const char **s;
    } ptr;
} FlagDef;

FlagDef *flag_defs;

void add_flag_bool(const char *name, bool *ptr, const char *help) {
    buf_push(flag_defs, (FlagDef){.kind = FLAG_BOOL, .name = name, .help = help, .ptr.b = ptr});
}

void add_flag_str(const char *name, const char **ptr, const char *arg_name, const char *help) {
    buf_push(flag_defs, (FlagDef){.kind = FLAG_STR, .name = name, .help = help, .arg_name = arg_name, .ptr.s = ptr});
}

void add_flag_enum(const char *name, int *ptr, const char *help, const char **options, int num_options) {
    buf_push(flag_defs, (FlagDef){.kind = FLAG_ENUM, .name = name, .help = help, .ptr.i = ptr, .options = options, .num_options = num_options});
}

FlagDef *get_flag_def(const char *name) {
    for (size_t i = 0; i < buf_len(flag_defs); i++) {
        if (strcmp(flag_defs[i].name, name) == 0) {
            return &flag_defs[i];
        }
    }
    return NULL;
}

void print_flags_usage(void) {
    printf("Flags:\n");
    for (size_t i = 0; i < buf_len(flag_defs); i++) {
        FlagDef flag = flag_defs[i];
        char note[256] = {0};
        char format[256];
        switch (flag.kind) {
        case FLAG_STR:
            snprintf(format, sizeof(format), "%s <%s>", flag.name, flag.arg_name ? flag.arg_name : "value");
            if (*flag.ptr.s) {
                snprintf(note, sizeof(note), "(default: %s)", *flag.ptr.s);
            }
            break;
        case FLAG_ENUM: {
            char *end = format + sizeof(format);
            char *ptr = format;
            ptr += snprintf(ptr, end - ptr, "%s <", flag.name);
            for (int k = 0; k < flag.num_options; k++) {
                ptr += snprintf(ptr, end - ptr, "%s%s", k == 0 ? "" : "|", flag.options[k]);
                if (k == *flag.ptr.i) {
                    snprintf(note, sizeof(note), " (default: %s)", flag.options[k]);
                }
            }
            snprintf(ptr, end - ptr, ">");
            break;
        }
        case FLAG_BOOL:
        default:
            snprintf(format, sizeof(format), "%s", flag.name);
            break;
        }
        printf(" -%-32s %s%s\n", format, flag.help ? flag.help : "", note);
    }
}

const char *parse_flags(int *argc_ptr, const char ***argv_ptr) {
    int argc = *argc_ptr;
    const char **argv = *argv_ptr;
    int i;
    for (i = 1; i < argc; i++) {
        const char *arg = argv[i];
        const char *name = arg;
        if (*name== '-') {
            name++;
            if (*name== '-') {
                name++;
            }
            FlagDef *flag = get_flag_def(name);
            if (!flag) {
                printf("Unknown flag %s\n", arg);
                continue;
            }
            switch (flag->kind) {
            case FLAG_BOOL:
                *flag->ptr.b = true;
                break;
            case FLAG_STR:
                if (i + 1 < argc) {
                    i++;
                    *flag->ptr.s = argv[i];
                } else {
                    printf("No value argument after -%s\n", arg);
                }
                break;
            case FLAG_ENUM: {
                const char *option;
                if (i + 1 < argc) {
                    i++;
                    option = argv[i];
                } else {
                    printf("No value after %s\n", arg);
                    break;
                }
                bool found = false;
                for (int k = 0; k < flag->num_options; k++) {
                    if (strcmp(flag->options[k], option) == 0) {
                        *flag->ptr.i = k;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    printf("Invalid value '%s' for %s\n", option, arg);
                }
                break;
            }
            default:
                printf("Unhandled flag kind\n");
                break;
            }
        } else {
            break;
        }
    }
    *argc_ptr = argc - i;
    *argv_ptr = argv + i;
    return path_file(strdup(argv[0]));
}
