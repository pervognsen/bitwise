bool ion_compile_file(const char *path) {
    char *str = read_file(path);
    if (!str) {
        return false;
    }
    init_stream(path, str);
    init_builtins();
    global_decls = parse_file();
    sym_global_decls();
    finalize_syms();
    gen_all();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    const char *c_path = replace_ext(path, "c");
    if (!c_path) {
        return false;
    }
    if (!write_file(c_path, c_code, buf_len(c_code))) {
        return false;
    }
    return true;
}

const char *ion_compile_str(const char *str) {
    init_stream(NULL, str);
    init_builtins();
    sym_global_decls(parse_file());
    finalize_syms();
    gen_all();
    const char *result = gen_buf;
    gen_buf = NULL;
    return result;
}
int ion_main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <ion-source-file>\n", argv[0]);
        return 1;
    }
    init_keywords();
    const char *path = argv[1];
    if (!ion_compile_file(path)) {
        printf("Compilation failed.\n");
        return 1;
    }
    printf("Compilation succeeded.\n");
    return 0;
}
