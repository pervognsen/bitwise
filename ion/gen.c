// This file is in rapid flux, so there's not much point in reporting bugs yet.

char *gen_buf = NULL;

#define genf(...) buf_printf(gen_buf, __VA_ARGS__)
#define genlnf(...) (genln(), genf(__VA_ARGS__))

int gen_indent;
SrcPos gen_pos;

const char *gen_preamble =
    "// Preamble\n"
    "#include <stdio.h>\n"
    "#include <stdbool.h>\n"
    "#include <math.h>\n"
    "\n"
    "typedef unsigned char uchar;\n"
    "typedef signed char schar;\n"
    "typedef unsigned short ushort;\n"
    "typedef unsigned int uint;\n"
    "typedef unsigned long ulong;\n"
    "typedef long long llong;\n"
    "typedef unsigned long long ullong;\n"
    "\n"
    "typedef uchar uint8;\n"
    "typedef schar int8;\n"
    "typedef ushort uint16;\n"
    "typedef short int16;\n"
    "typedef uint uint32;\n"
    "typedef int int32;\n"
    "typedef ullong uint64;\n"
    "typedef llong int64;\n"
    "\n"
    ;

void genln(void) {
    genf("\n%.*s", gen_indent * 4, "                                                                  ");
    gen_pos.line++;
}

bool is_incomplete_array_typespec(Typespec *typespec) {
    return typespec->kind == TYPESPEC_ARRAY && !typespec->num_elems;
}

char char_to_escape[256] = {
    ['\0'] = '0',
    ['\n'] = 'n',
    ['\r'] = 'r',
    ['\t'] = 't',
    ['\v'] = 'v',
    ['\b'] = 'b',
    ['\a'] = 'a',
    ['\\'] = '\\',
    ['"'] = '"',
    ['\''] = '\'',
};

void gen_char(char c) {
    if (char_to_escape[(unsigned char)c]) {
        genf("'\\%c'", char_to_escape[(unsigned char)c]);
    } else if (isprint(c)) {
        genf("'%c'", c);
    } else {
        genf("'\\x%x'", c);
    }
}

void gen_str(const char *str) {
    genf("\"");
    while (*str) {
        const char *start = str;
        while (*str && isprint(*str) && !char_to_escape[(unsigned char)*str]) {
            str++;
        }
        if (start != str) {
            genf("%.*s", str - start, start);
        }
        if (*str) {
            if (char_to_escape[(unsigned char)*str]) {
                genf("\\%c", char_to_escape[(unsigned char)*str]);
            } else {
                assert(!isprint(*str));
                genf("\\x%x", *str);
            }
            str++;
        }
    }
    genf("\"");
}

void gen_sync_pos(SrcPos pos) {
    if (gen_pos.line != pos.line || gen_pos.name != pos.name) {
        genlnf("#line %d", pos.line);
        if (gen_pos.name != pos.name) {
            genf(" ");
            gen_str(pos.name);
        }
        gen_pos = pos;
    }
}

const char *cdecl_paren(const char *str, bool b) {
    return b ? strf("(%s)", str) : str;
}

const char *cdecl_name(Type *type) {
    const char *type_name = type_names[type->kind];
    if (type_name) {
        return type_name;
    } else {
        assert(type->sym);
        return type->sym->name;
    }
}

char *type_to_cdecl(Type *type, const char *str) {
    switch (type->kind) {
    case TYPE_PTR:
        return type_to_cdecl(type->base, cdecl_paren(strf("*%s", str), *str));
    case TYPE_CONST:
        return type_to_cdecl(type->base, strf("const %s", cdecl_paren(str, *str)));
    case TYPE_ARRAY:
        if (type->num_elems == 0) {
            return type_to_cdecl(type->base, cdecl_paren(strf("%s[]", str), *str));
        } else {
            return type_to_cdecl(type->base, cdecl_paren(strf("%s[%llu]", str, type->num_elems), *str));
        }
    case TYPE_FUNC: {
        char *result = NULL;
        buf_printf(result, "%s(", cdecl_paren(strf("*%s", str), *str));
        if (type->func.num_params == 0) {
            buf_printf(result, "void");
        } else {
            for (size_t i = 0; i < type->func.num_params; i++) {
                buf_printf(result, "%s%s", i == 0 ? "" : ", ", type_to_cdecl(type->func.params[i], ""));
            }
        }
        if (type->func.has_varargs) {
            buf_printf(result, ", ...");
        }
        buf_printf(result, ")");
        return type_to_cdecl(type->func.ret, result);
    }
    default:
        return strf("%s%s%s", cdecl_name(type), *str ? " " : "", str);
    }
}

void gen_expr(Expr *expr);

const char *gen_expr_str(Expr *expr) {
    char *temp = gen_buf;
    gen_buf = NULL;
    gen_expr(expr);
    const char *result = gen_buf;
    gen_buf = temp;
    return result;
}

char *typespec_to_cdecl(Typespec *typespec, const char *str) {
    // TODO: Figure out how to handle type vs typespec in C gen for inferred types. How to prevent "flattened" const values?
    switch (typespec->kind) {
    case TYPESPEC_NAME:
        return strf("%s%s%s", typespec->name, *str ? " " : "", str);
    case TYPESPEC_PTR:
        return typespec_to_cdecl(typespec->base, cdecl_paren(strf("*%s", str), *str));
    case TYPESPEC_CONST:
        return typespec_to_cdecl(typespec->base, strf("const %s", cdecl_paren(str, *str)));
    case TYPESPEC_ARRAY:
        if (typespec->num_elems == 0) {
            return typespec_to_cdecl(typespec->base, cdecl_paren(strf("%s[]", str), *str));
        } else {
            return typespec_to_cdecl(typespec->base, cdecl_paren(strf("%s[%s]", str, gen_expr_str(typespec->num_elems)), *str));
        }
    case TYPESPEC_FUNC: {
        char *result = NULL;
        buf_printf(result, "%s(", cdecl_paren(strf("*%s", str), *str));
        if (typespec->func.num_args == 0) {
            buf_printf(result, "void");
        } else {
            for (size_t i = 0; i < typespec->func.num_args; i++) {
                buf_printf(result, "%s%s", i == 0 ? "" : ", ", typespec_to_cdecl(typespec->func.args[i], ""));
            }
        }
        if (typespec->func.has_varargs) {
            buf_printf(result, ", ...");
        }
        buf_printf(result, ")");
        return typespec_to_cdecl(typespec->func.ret, result);
    }
    default:
        assert(0);
        return NULL;
    }
}

void gen_func_decl(Decl *decl) {
    assert(decl->kind == DECL_FUNC);
    gen_sync_pos(decl->pos);
    if (decl->func.ret_type) {
        genlnf("%s(", typespec_to_cdecl(decl->func.ret_type, decl->name));
    } else {
        genlnf("void %s(", decl->name);
    }
    if (decl->func.num_params == 0) {
        genf("void");
    } else {
        for (size_t i = 0; i < decl->func.num_params; i++) {
            FuncParam param = decl->func.params[i];
            if (i != 0) {
                genf(", ");
            }
            genf("%s", typespec_to_cdecl(param.type, param.name));
        }
    }
    if (decl->func.has_varargs) {
        genf(", ...");
    }
    genf(")");
}

void gen_forward_decls(void) {
    for (Sym **it = global_syms_buf; it != buf_end(global_syms_buf); it++) {
        Sym *sym = *it;
        Decl *decl = sym->decl;
        if (!decl) {
            continue;
        }
        switch (decl->kind) {
        case DECL_STRUCT:
            genlnf("typedef struct %s %s;", sym->name, sym->name);
            break;
        case DECL_UNION:
            genlnf("typedef union %s %s;", sym->name, sym->name);
            break;
        default:
            // Do nothing.
            break;
        }
    }
}

void gen_aggregate(Decl *decl) {
    assert(decl->kind == DECL_STRUCT || decl->kind == DECL_UNION);
    genlnf("%s %s {", decl->kind == DECL_STRUCT ? "struct" : "union", decl->name);
    gen_indent++;
    for (size_t i = 0; i < decl->aggregate.num_items; i++) {
        AggregateItem item = decl->aggregate.items[i];
        for (size_t j = 0; j < item.num_names; j++) {
            gen_sync_pos(item.pos);
            genlnf("%s;", typespec_to_cdecl(item.type, item.names[j]));
        }
    }
    gen_indent--;
    genlnf("};");
}

void gen_expr_compound(Expr *expr, bool is_init) {
    if (is_init) {
        genf("{");
    } else if (expr->compound.type) {
        genf("(%s){", typespec_to_cdecl(expr->compound.type, ""));
    } else {
        genf("(%s){", type_to_cdecl(expr->type, ""));
    }
    for (size_t i = 0; i < expr->compound.num_fields; i++) {
        if (i != 0) {
            genf(", ");
        }
        CompoundField field = expr->compound.fields[i];
        if (field.kind == FIELD_NAME) {
            genf(".%s = ", field.name);
        } else if (field.kind == FIELD_INDEX) {
            genf("[");
            gen_expr(field.index);
            genf("] = ");
        }
        gen_expr(field.init);
    }
    if (expr->compound.num_fields == 0) {
        genf("0");
    }
    genf("}");
}

void gen_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_INT: {
        const char *suffix_name = token_suffix_names[expr->int_lit.suffix];
        switch (expr->int_lit.mod) {
        case MOD_BIN:
        case MOD_HEX:
            genf("0x%llx%s", expr->int_lit.val, suffix_name);
            break;
        case MOD_OCT:
            genf("0%llo%s", expr->int_lit.val, suffix_name);
            break;
        case MOD_CHAR:
            gen_char((char)expr->int_lit.val);
            break;
        default:
            genf("%llu%s", expr->int_lit.val, suffix_name);
            break;
        }
        break;
    }
    case EXPR_FLOAT:
        genf("%f%s", expr->float_lit.val, expr->float_lit.suffix == SUFFIX_D ? "" : "f");
        break;
    case EXPR_STR:
        gen_str(expr->str_val);
        break;
    case EXPR_NAME:
        genf("%s", expr->name);
        break;
    case EXPR_CAST:
        genf("(%s)(", type_to_cdecl(expr->cast.type->type, ""));
        gen_expr(expr->cast.expr);
        genf(")");
        break;
    case EXPR_CALL:
        genf("(");
        gen_expr(expr->call.expr);
        genf(")");
        genf("(");
        for (size_t i = 0; i < expr->call.num_args; i++) {
            if (i != 0) {
                genf(", ");
            }
            gen_expr(expr->call.args[i]);
        }
        genf(")");
        break;
    case EXPR_INDEX:
        gen_expr(expr->index.expr);
        genf("[");
        gen_expr(expr->index.index);
        genf("]");
        break;
    case EXPR_FIELD:
        gen_expr(expr->field.expr);
        genf("%s%s", expr->field.expr->type->kind == TYPE_PTR ? "->" : ".", expr->field.name);
        break;
    case EXPR_COMPOUND:
        gen_expr_compound(expr, false);
        break;
    case EXPR_UNARY:
        genf("%s(", token_kind_name(expr->unary.op));
        gen_expr(expr->unary.expr);
        genf(")");
        break;
    case EXPR_BINARY:
        genf("(");
        gen_expr(expr->binary.left);
        genf(") %s (", token_kind_name(expr->binary.op));
        gen_expr(expr->binary.right);
        genf(")");
        break;
    case EXPR_TERNARY:
        genf("(");
        gen_expr(expr->ternary.cond);
        genf(" ? ");
        gen_expr(expr->ternary.then_expr);
        genf(" : ");
        gen_expr(expr->ternary.else_expr);
        genf(")");
        break;
    case EXPR_SIZEOF_EXPR:
        genf("sizeof(");
        gen_expr(expr->sizeof_expr);
        genf(")");
        break;
    case EXPR_SIZEOF_TYPE:
        genf("sizeof(%s)", type_to_cdecl(expr->sizeof_type->type, ""));
        break;
    default:
        assert(0);
    }
}

void gen_stmt(Stmt *stmt);

void gen_stmt_block(StmtList block) {
    genf("{");
    gen_indent++;
    for (size_t i = 0; i < block.num_stmts; i++) {
        gen_stmt(block.stmts[i]);
    }
    gen_indent--;
    genlnf("}");
}

void gen_init_expr(Expr *expr) {
    if (expr->kind == EXPR_COMPOUND) {
        gen_expr_compound(expr, true);
    } else {
        gen_expr(expr);
    }
}

void gen_simple_stmt(Stmt *stmt) {
    switch (stmt->kind) {
    case STMT_EXPR:
        gen_expr(stmt->expr);
        break;
    case STMT_INIT:
        if (stmt->init.type) {
            if (is_incomplete_array_typespec(stmt->init.type)) {
                genf("%s", type_to_cdecl(stmt->init.expr->type, stmt->init.name));
            } else {
                genf("%s", typespec_to_cdecl(stmt->init.type, stmt->init.name));
            }
            if (stmt->init.expr) {
                genf(" = ");
                gen_init_expr(stmt->init.expr);
            }
        } else {
            genf("%s = ", type_to_cdecl(unqualify_type(stmt->init.expr->type), stmt->init.name));
            gen_init_expr(stmt->init.expr);
        }
        break;
    case STMT_ASSIGN:
        gen_expr(stmt->assign.left);
        if (stmt->assign.right) {
            genf(" %s ", token_kind_name(stmt->assign.op));
            gen_expr(stmt->assign.right);
        } else {
            genf("%s", token_kind_name(stmt->assign.op));
        }
        break;
    default:
        assert(0);
    }
}

void gen_stmt(Stmt *stmt) {
    gen_sync_pos(stmt->pos);
    switch (stmt->kind) {
    case STMT_RETURN:
        genlnf("return");
        if (stmt->expr) {
            genf(" ");
            gen_expr(stmt->expr);
        }
        genf(";");
        break;
    case STMT_BREAK:
        genlnf("break;");
        break;
    case STMT_CONTINUE:
        genlnf("continue;");
        break;
    case STMT_BLOCK:
        genln();
        gen_stmt_block(stmt->block);
        break;
    case STMT_IF:
        genlnf("if (");
        gen_expr(stmt->if_stmt.cond);
        genf(") ");
        gen_stmt_block(stmt->if_stmt.then_block);
        for (size_t i = 0; i < stmt->if_stmt.num_elseifs; i++) {
            ElseIf elseif = stmt->if_stmt.elseifs[i];
            genf(" else if (");
            gen_expr(elseif.cond);
            genf(") ");
            gen_stmt_block(elseif.block);
        }
        if (stmt->if_stmt.else_block.stmts) {
            genf(" else ");
            gen_stmt_block(stmt->if_stmt.else_block);
        }
        break;
    case STMT_WHILE:
        genlnf("while (");
        gen_expr(stmt->while_stmt.cond);
        genf(") ");
        gen_stmt_block(stmt->while_stmt.block);
        break;
    case STMT_DO_WHILE:
        genlnf("do ");
        gen_stmt_block(stmt->while_stmt.block);
        genf(" while (");
        gen_expr(stmt->while_stmt.cond);
        genf(");");
        break;
    case STMT_FOR:
        genlnf("for (");
        if (stmt->for_stmt.init) {
            gen_simple_stmt(stmt->for_stmt.init);
        }
        genf(";");
        if (stmt->for_stmt.cond) {
            genf(" ");
            gen_expr(stmt->for_stmt.cond);
        }
        genf(";");
        if (stmt->for_stmt.next) {
            genf(" ");
            gen_simple_stmt(stmt->for_stmt.next);
        }
        genf(") ");
        gen_stmt_block(stmt->for_stmt.block);
        break;
    case STMT_SWITCH:
        genlnf("switch (");
        gen_expr(stmt->switch_stmt.expr);
        genf(") {");
        for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
            SwitchCase switch_case = stmt->switch_stmt.cases[i];
            for (size_t j = 0; j < switch_case.num_exprs; j++) {
                genlnf("case ");
                gen_expr(switch_case.exprs[j]);
                genf(":");

            }
            if (switch_case.is_default) {
                genlnf("default:");
            }
            genf(" ");
            genf("{");
            gen_indent++;
            StmtList block = switch_case.block;
            for (size_t j = 0; i < block.num_stmts; i++) {
                gen_stmt(block.stmts[j]);
            }
            genlnf("break;");
            gen_indent--;
            genlnf("}");
        }
        genlnf("}");
        break;
    default:
        genln();
        gen_simple_stmt(stmt);
        genf(";");
        break;
    }
}

void gen_decl(Sym *sym) {
    Decl *decl = sym->decl;
    if (!decl || is_decl_foreign(decl)) {
        return;
    }
    gen_sync_pos(decl->pos);
    switch (decl->kind) {
    case DECL_CONST:
        genlnf("#define %s (", sym->name);
        gen_expr(decl->const_decl.expr);
        genf(")");
        break;
    case DECL_VAR:
        if (decl->var.type && !is_incomplete_array_typespec(decl->var.type)) {
            genlnf("%s", typespec_to_cdecl(decl->var.type, sym->name));
        } else {
            genlnf("%s", type_to_cdecl(sym->type, sym->name));
        }
        if (decl->var.expr) {
            genf(" = ");
            gen_init_expr(decl->var.expr);
        }
        genf(";");
        break;
    case DECL_FUNC:
        gen_func_decl(decl);
        genf(";");
        break;
    case DECL_STRUCT:
    case DECL_UNION:
        gen_aggregate(decl);
        break;
    case DECL_TYPEDEF:
        genlnf("typedef %s;", typespec_to_cdecl(decl->typedef_decl.type, sym->name));
        break;
    default:
        assert(0);
        break;
    }
    genln();
}

void gen_sorted_decls(void) {
    for (size_t i = 0; i < buf_len(sorted_syms); i++) {
        gen_decl(sorted_syms[i]);
    }
}

void cdecl_test(void) {
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

void gen_func_defs(void) {
    for (Sym **it = global_syms_buf; it != buf_end(global_syms_buf); it++) {
        Sym *sym = *it;
        Decl *decl = sym->decl;
        if (decl && decl->kind == DECL_FUNC && !is_decl_foreign(decl)) {
            gen_func_decl(decl);
            genf(" ");
            gen_stmt_block(decl->func.block);
            genln();
        }
    }
}

void gen_all(void) {
    gen_buf = NULL;
    genf("%s", gen_preamble);
    genf("// Forward declarations");
    gen_forward_decls();
    genln();
    genlnf("// Sorted declarations");
    gen_sorted_decls();
    genlnf("// Function definitions");
    gen_func_defs();
}
