enum { MAX_SEARCH_PATHS = 256 };
const char *static_package_search_paths[MAX_SEARCH_PATHS];
const char **package_search_paths = static_package_search_paths;
int num_package_search_paths;

void add_package_search_path(const char *path) {
    if (flag_verbose) {
        printf("Adding package search path %s\n", path);
    }
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
    init_target();
    init_package_search_paths();
    init_keywords();
    init_builtin_types();
    map_put(&decl_note_names, declare_note_name, (void *)1);
}

void parse_env_vars(void) {
    const char *ionos_var = getenv("IONOS");
    if (ionos_var) {
        int os = get_os(ionos_var);
        if (os == -1) {
            printf("Unknown target operating system in IONOS environment variable: %s\n", ionos_var);
        } else {
            target_os = os;
        }
    }
    const char *ionarch_var = getenv("IONARCH");
    if (ionarch_var) {
        int arch = get_arch(ionarch_var);
        if (arch == -1) {
            printf("Unknown target architecture in IONARCH environment variable: %s\n", ionarch_var);
        } else {
            target_arch = arch;
        }
    }
}

int ion_main(int argc, const char **argv) {
    parse_env_vars();
    const char *output_name = NULL;
    bool flag_check = false;
    add_flag_str("o", &output_name, "file", "Output file (default: out_<main-package>.c)");
    add_flag_enum("os", &target_os, "Target operating system", os_names, NUM_OSES);
    add_flag_enum("arch", &target_arch, "Target machine architecture", arch_names, NUM_ARCHES);
    add_flag_bool("check", &flag_check, "Semantic checking with no code generation");
    add_flag_bool("lazy", &flag_lazy, "Only compile what's reachable from the main package");
    add_flag_bool("notypeinfo", &flag_notypeinfo, "Don't generate any typeinfo tables");
    add_flag_bool("fullgen", &flag_fullgen, "Force full code generation even for non-reachable symbols");
    add_flag_bool("verbose", &flag_verbose, "Extra diagnostic information");
    const char *program_name = parse_flags(&argc, &argv);
    if (argc != 1) {
        printf("Usage: %s [flags] <main-package>\n", program_name);
        print_flags_usage();
        return 1;
    }
    const char *package_name = argv[0];
    if (flag_verbose) {
        printf("Target operating system: %s\n", os_names[target_os]);
        printf("Target architecture: %s\n", arch_names[target_arch]);
    }
    init_compiler();
    builtin_package = import_package("builtin");
    if (!builtin_package) {
        printf("error: Failed to compile package 'builtin'.\n");
        return 1;
    }
    builtin_package->external_name = str_intern("");
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
    reachable_phase = REACHABLE_NATURAL;
    resolve_sym(main_sym);
    for (size_t i = 0; i < buf_len(package_list); i++) {
        if (package_list[i]->always_reachable) {
            resolve_package_syms(package_list[i]);
        }
    }
    finalize_reachable_syms();
    if (flag_verbose) {
        printf("Reached %d symbols in %d packages from %s/main\n", (int)buf_len(reachable_syms), (int)buf_len(package_list), package_name);
    }
    if (!flag_lazy) {
        reachable_phase = REACHABLE_FORCED;
        for (size_t i = 0; i < buf_len(package_list); i++) {
            resolve_package_syms(package_list[i]);
        }
        finalize_reachable_syms();
    }
    printf("Processed %d symbols in %d packages\n", (int)buf_len(reachable_syms), (int)buf_len(package_list));
    if (!flag_check) {
        char c_path[MAX_PATH];
        if (output_name) {
            path_copy(c_path, output_name);
        } else {
            snprintf(c_path, sizeof(c_path), "out_%s.c", package_name);
        }
        gen_all();
        const char *c_code = gen_buf;
        gen_buf = NULL;
        if (!write_file(c_path, c_code, buf_len(c_code))) {
            printf("error: Failed to write file: %s\n", c_path);
            return 1;
        }
        printf("Generated %s\n", c_path);
    }
    return 0;
}
