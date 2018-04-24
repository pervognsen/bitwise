void intern_test(void) {
    char a[] = "hello";
    assert(strcmp(a, str_intern(a)) == 0);
    assert(str_intern(a) == str_intern(a));
    assert(str_intern(str_intern(a)) == str_intern(a));
    char b[] = "hello";
    assert(a != b);
    assert(str_intern(a) == str_intern(b));
    char c[] = "hello!";
    assert(str_intern(a) != str_intern(c));
    char d[] = "hell";
    assert(str_intern(a) != str_intern(d));
}

void common_test(void) {
    buf_test();
    intern_test();
    map_test();

    char *str1 = strf("%d %d", 1, 2);
    assert(strcmp(str1, "1 2") == 0);
    char *str2 = strf("%s %s", str1, str1);
    assert(strcmp(str2, "1 2 1 2") == 0);
    char *str3 = strf("%s asdf %s", str2, str2);
    assert(strcmp(str3, "1 2 1 2 asdf 1 2 1 2") == 0);
}

void keyword_test(void) {
    init_keywords();
    assert(is_keyword_name(first_keyword));
    assert(is_keyword_name(last_keyword));
    for (const char **it = keywords; it != buf_end(keywords); it++) {
        assert(is_keyword_name(*it));
    }
    assert(!is_keyword_name(str_intern("foo")));
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

void lex_test(void) {
    keyword_test();
    assert(str_intern("func") == func_keyword);

    // Integer literal tests
    init_stream(NULL, "0 2147483647 0x7fffffff 042 0b1111");
    assert_token_int(0);
    assert_token_int(2147483647);
    assert(token.mod == MOD_HEX);
    assert_token_int(0x7fffffff);
    assert(token.mod == MOD_OCT);
    assert_token_int(042);
    assert(token.mod == MOD_BIN);
    assert_token_int(0xF);
    assert_token_eof();

    // Float literal tests
    init_stream(NULL, "3.14 .123 42. 3e10");
    assert_token_float(3.14);
    assert_token_float(.123);
    assert_token_float(42.);
    assert_token_float(3e10);
    assert_token_eof();

    // Char literal tests
    init_stream(NULL, "'a' '\\n'");
    assert_token_int('a');
    assert_token_int('\n');
    assert_token_eof();

    // String literal tests
    init_stream(NULL, "\"foo\" \"a\\nb\"");
    assert_token_str("foo");
    assert_token_str("a\nb");
    assert_token_eof();

    // Operator tests
    init_stream(NULL, ": := + += ++ < <= << <<=");
    assert_token(TOKEN_COLON);
    assert_token(TOKEN_COLON_ASSIGN);
    assert_token(TOKEN_ADD);
    assert_token(TOKEN_ADD_ASSIGN);
    assert_token(TOKEN_INC);
    assert_token(TOKEN_LT);
    assert_token(TOKEN_LTEQ);
    assert_token(TOKEN_LSHIFT);
    assert_token(TOKEN_LSHIFT_ASSIGN);
    assert_token_eof();

    // Misc tests
    init_stream(NULL, "XY+(XY)_HELLO1,234+994");
    assert_token_name("XY");
    assert_token(TOKEN_ADD);
    assert_token(TOKEN_LPAREN);
    assert_token_name("XY");
    assert_token(TOKEN_RPAREN);
    assert_token_name("_HELLO1");
    assert_token(TOKEN_COMMA);
    assert_token_int(234);
    assert_token(TOKEN_ADD);
    assert_token_int(994);
    assert_token_eof();
}

#undef assert_token
#undef assert_token_name
#undef assert_token_int
#undef assert_token_float
#undef assert_token_str
#undef assert_token_eof

void parse_test(void) {
    const char *decls[] = {
        "var x: char[256] = {1, 2, 3, ['a'] = 4}",
        "struct Vector { x, y: float; }",
        "var v = Vector{x = 1.0, y = -1.0}",
        "var v: Vector = {1.0, -1.0}",
        "const n = sizeof(:int*[16])",
        "const n = sizeof(1+2)",
        "var x = b == 1 ? 1+2 : 3-4",
        "func fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }",
        "func fact(n: int): int { p := 1; for (i := 1; i <= n; i++) { p *= i; } return p; }",
        "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0",
        "func f(x: int): bool { switch (x) { case 0: case 1: return true; case 2: default: return false; } }",
        "enum Color { RED = 3, GREEN, BLUE = 0 }",
        "const pi = 3.14",
        "union IntOrFloat { i: int; f: float; }",
        "typedef Vectors = Vector[1+2]",
        "func f() { do { print(42); } while(1); }",
        "typedef T = (func(int):int)[16]",
        "func f() { enum E { A, B, C } return; }",
        "func f() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }",
};
    for (const char **it = decls; it != decls + sizeof(decls)/sizeof(*decls); it++) {
        init_stream(NULL, *it);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}

void gen_cdecl_test(void) {
#if 0
    char *cdecl1 = type_to_cdecl(type_int, "x");
    char *cdecl2 = type_to_cdecl(type_ptr(type_int), "x");
    char *cdecl3 = type_to_cdecl(type_array(type_int, 10), "x");
    char *cdecl4 = type_to_cdecl(type_func((Type*[]){type_int}, 1, type_int), "x");
    char *cdecl5 = type_to_cdecl(type_array(type_func((Type*[]){type_int}, 1, type_int), 10), "x");
    char *cdecl6 = type_to_cdecl(type_func((Type*[]){type_ptr(type_int)}, 1, type_int), "x");
    Type *type1 = type_func((Type*[]){type_array(type_int, 10)}, 1, type_int);
    char *cdecl7 = type_to_cdecl(type1, "x");
    char *cdecl8 = type_to_cdecl(type_func(NULL, 0, type1), "x");
    char *cdecl9 = type_to_cdecl(type_func(NULL, 0, type_array(type_func(NULL, 0, type_int), 10)), "x");
#endif
}

void resolve_test(void) {
    #if 0
    Type *int_ptr = type_ptr(type_int);
    assert(type_ptr(type_int) == int_ptr);
    Type *float_ptr = type_ptr(type_float);
    assert(type_ptr(type_float) == float_ptr);
    assert(int_ptr != float_ptr);
    Type *int_ptr_ptr = type_ptr(type_ptr(type_int));
    assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);
    Type *float4_array = type_array(type_float, 4);
    assert(type_array(type_float, 4) == float4_array);
    Type *float3_array = type_array(type_float, 3);
    assert(type_array(type_float, 3) == float3_array);
    assert(float4_array != float3_array);
    Type *int_int_func = type_func(&type_int, 1, type_int, false);
    assert(type_func(&type_int, 1, type_int, false) == int_int_func);
    Type *int_func = type_func(NULL, 0, type_int, false);
    assert(int_int_func != int_func);
    assert(int_func == type_func(NULL, 0, type_int, false));

    init_builtins();

    const char *code[] = {
        "union IntOrPtr { i: int; p: int*; }",
        "var u1 = IntOrPtr{i = 42}",
        "var u2 = IntOrPtr{p = (:int*)42}",
        "var i: int",
        "struct Vector { x, y: int; }",
        "func f1() { v := Vector{1, 2}; j := i; i++; j++; v.x = 2*j; }",
        "func f2(n: int): int { return 2*n; }",
        "func f3(x: int): int { if (x) { return -x; } else if (x % 2 == 0) { return 42; } else { return -1; } }",
        "func f4(n: int): int { for (i := 0; i < n; i++) { if (i % 3 == 0) { return n; } } return 0; }",
        "func f5(x: int): int { switch(x) { case 0, 1: return 42; case 3: default: return -1; } }",
        "func f6(n: int): int { p := 1; while (n) { p *= 2; n--; } return p; }",
        "func f7(n: int): int { p := 1; do { p *= 2; n--; } while (n); return p; }",
        /*
        "var i: int",
        "func add(v: Vector, w: Vector): Vector { return {v.x + w.x, v.y + w.y}; }",
        "var a: int[256] = {1, 2, ['a'] = 42, [255] = 123}",
        "var v: Vector = 0 ? {1,2} : {3,4}",
        "var vs: Vector[2][2] = {{{1,2},{3,4}}, {{5,6},{7,8}}}",
        "struct A { c: char; }",
        "struct B { i: int; }",
        "struct C { c: char; a: A; }",
        "struct D { c: char; b: B; }",
        "func print(v: Vector) { printf(\"{%d, %d}\", v.x, v.y); }",
        "var x = add({1,2}, {3,4})",
        "var v: Vector = {1,2}",
        "var w = Vector{3,4}",
        "var p: void*",
        "var i = (:int)p + 1",
        "var fp: func(Vector)",
        "struct Dup { x: int; x: int; }",
        "var a: int[3] = {1,2,3}",
        "var b: int[4]",
        "var p = &a[1]",
        "var i = p[1]",
        "var j = *p",
        "const n = sizeof(a)",
        "const m = sizeof(&a[0])",
        "const l = sizeof(1 ? a : b)",
        "var pi = 3.14",
        "var name = \"Per\"",
        "var v = Vector{1,2}",
        "var j = (:int)p",
        "var q = (:int*)j",
        "const i = 42",
        "const j = +i",
        "const k = -i",
        "const a = 1000/((2*3-5) << 1)",
        "const b = !0",
        "const c = ~100 + 1 == -100",
        "const k = 1 ? 2 : 3",
        "union IntOrPtr { i: int; p: int*; }",
        "var i = 42",
        "var u = IntOrPtr{i, &i}",
        "const n = 1+sizeof(p)",
        "var p: T*",
        "var u = *p",
        "struct T { a: int[n]; }",
        "var r = &t.a",
        "var t: T",
        "typedef S = int[n+m]",
        "const m = sizeof(t.a)",
        "var i = n+m",
        "var q = &i",
        "const n = sizeof(x)",
        "var x: T",
        "struct T { s: S*; }",
        "struct S { t: T[n]; }",
        */
    };
    for (size_t i = 0; i < sizeof(code)/sizeof(*code); i++) {
        init_stream(NULL, code[i]);
        Decl *decl = parse_decl();
        sym_global_decl(decl);
    }
    finalize_global_syms();
    for (Sym **it = sorted_syms; it != buf_end(sorted_syms); it++) {
        Sym *sym = *it;
        if (sym->decl) {
            print_decl(sym->decl);
        } else {
            printf("%s", sym->name);
        }
        printf("\n");
    }
    #endif
}

void main_test(void) {
    // common_test();
    // lex_test();
    // print_test();
    // parse_test();
    // resolve_test();
    // ion_test();
}
