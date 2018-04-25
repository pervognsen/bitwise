enum { MAX_SEARCH_PATHS = 256 };
const char *static_package_search_paths[MAX_SEARCH_PATHS];
const char **package_search_paths = static_package_search_paths;
int num_package_search_paths;

void add_package_search_path(const char *path) {
    printf("Adding package search path %s\n", path);
    package_search_paths[num_package_search_paths++] = str_intern(path);
}

void add_package_search_path_range(const char *start, const char *end) {
    char path[MAX_PATH];
    size_t len = CLAMP_MAX(end - start, MAX_PATH - 1);
    memcpy(path, start, len);
    path[len] = 0;
    add_package_search_path(path);
}

void init_package_search_paths(void) {
    const char *ionhome_var = getenv("IONHOME");
    if (!ionhome_var) {
        printf("error: Set the environment variable IONHOME to the Ion home directory (where system_packages is located)\n");
        exit(1);
    }
    char path[MAX_PATH];
    path_copy(path, ionhome_var);
    path_join(path, "system_packages");
    add_package_search_path(path);
    add_package_search_path(".");
    const char *ionpath_var = getenv("IONPATH");
    if (ionpath_var) {
        const char *start = ionpath_var;
        for (const char *ptr = ionpath_var; *ptr; ptr++) {
            if (*ptr == ';') {
                add_package_search_path_range(start, ptr);
                start = ptr + 1;
            }
        }
        if (*start) {
            add_package_search_path(start);
        }
    }
}

void init_compiler(void) {
    init_package_search_paths();
    init_keywords();
    init_builtin_types();
    map_put(&decl_note_names, declare_note_name, (void *)1);
}

int ion_main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <package> [<output-c-file>]\n", argv[0]);
        return 1;
    }
    const char *package_name = argv[1];
    init_compiler();

    builtin_package = import_package("builtin");
    if (!builtin_package) {
        printf("error: Failed to compile package 'builtin'.\n");
        return 1;
    }
    builtin_package->external_name = str_intern("");
    init_builtin_syms();
    Package *main_package = import_package(package_name);
    if (!main_package) {
        printf("error: Failed to compile package '%s'\n", package_name);
        return 1;
    }
    const char *main_name = str_intern("main");
    Sym *main_sym = get_package_sym(main_package, main_name);
    if (!main_sym) {
        printf("error: No 'main' entry point defined in package '%s'\n", package_name);
        return 1;
    }
    main_sym->external_name = main_name;
    resolve_package_syms(builtin_package);
    resolve_package_syms(main_package);
    // for (int i = 0; i < buf_len(package_list); i++) {
    //     resolve_package_syms(package_list[i]);
    // }
    finalize_reachable_syms();
    printf("Compiled %d symbols in %d packages\n", (int)buf_len(reachable_syms), (int)buf_len(package_list));
    char c_path[MAX_PATH];
    if (argc >= 3) {
        path_copy(c_path, argv[2]);
    } else {
        snprintf(c_path, sizeof(c_path), "out_%s.c", package_name);
    }
    printf("Generating %s\n", c_path);
    gen_all();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    if (!write_file(c_path, c_code, buf_len(c_code))) {
        printf("error: Failed to write file: %s\n", c_path);
        return 1;
    }
    return 0;
}
