const char *builtin_code =
    "#declare_note(foreign)\n"
    "\n"
    "enum TypeKind {\n"
    "    TYPE_NONE,\n"
    "    TYPE_VOID,\n"
    "    TYPE_BOOL,\n"
    "    TYPE_CHAR,\n"
    "    TYPE_UCHAR,\n"
    "    TYPE_SCHAR,\n"
    "    TYPE_SHORT,\n"
    "    TYPE_USHORT,\n"
    "    TYPE_INT,\n"
    "    TYPE_UINT,\n"
    "    TYPE_LONG,\n"
    "    TYPE_ULONG,\n"
    "    TYPE_LLONG,\n"
    "    TYPE_ULLONG,\n"
    "    TYPE_FLOAT,\n"
    "    TYPE_DOUBLE,\n"
    "    TYPE_CONST,\n"
    "    TYPE_PTR,\n"
    "    TYPE_ARRAY,\n"
    "    TYPE_STRUCT,\n"
    "    TYPE_UNION,\n"
    "    TYPE_FUNC,\n"
    "}\n"
    "\n"
    "struct TypeFieldInfo {\n"
    "    name: char const*;\n"
    "    type: typeid;\n"
    "    offset: int;\n"
    "}\n"
    "\n"
    "struct TypeInfo {\n"
    "    kind: TypeKind;\n"
    "    size: int;\n"
    "    align: int;\n"
    "    name: char const*;\n"
    "    count: int;\n"
    "    base: typeid;\n"
    "    fields: TypeFieldInfo*;\n"
    "    num_fields: int;\n"
    "}\n"
    "\n"
    "@foreign\n"
    "var typeinfos: TypeInfo const**;\n"
    "\n"
    "@foreign\n"
    "var num_typeinfos: int;\n"
    "\n"
    "func get_typeinfo(type: typeid): TypeInfo const* {\n"
    "    if (typeinfos && type < num_typeinfos) {\n"
    "        return typeinfos[type];\n"
    "    } else {\n"
    "        return NULL;\n"
    "    }\n"
    "}\n"
    "struct Any {\n"
    "    ptr: void*;\n"
    "    type: typeid;\n"
    "}\n"
    "";

void init_compiler(void) {
    init_builtins();
    init_types();
}

bool ion_compile_builtin(void) {
    init_stream("<builtin>", builtin_code);
    init_compiler();
    global_decls = parse_decls();
    sym_global_decls();
    return true;
}

bool ion_compile_file(const char *path) {
    char *str = read_file(path);
    if (!str) {
        printf("Failed to read %s\n", path);
        return false;
    }
    if (!ion_compile_builtin()) {
        printf("Failed to compile builtins\n");
        return false;
    }
    init_stream(path, str);
    init_compiler();
    global_decls = parse_decls();
    sym_global_decls();
    finalize_syms();
    gen_all();
    const char *c_code = gen_buf;
    gen_buf = NULL;
    const char *c_path = replace_ext(path, "c");
    if (!c_path) {
        printf("File does not have extension\n");
        return false;
    }
    if (!write_file(c_path, c_code, buf_len(c_code))) {
        printf("Failed to write file: %s\n", c_path);
        return false;
    }
    return true;
}

const char *ion_compile_str(const char *str) {
    init_stream(NULL, str);
    init_builtins();
    global_decls = parse_decls();
    sym_global_decls();
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
