typedef enum SymKind {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FUNC,
    SYM_TYPE,
} SymKind;

typedef enum SymState {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

struct Package;

typedef struct Sym {
    const char *name;
    struct Package *package;
    SymKind kind;
    SymState state;
    uint8_t reachable;
    Decl *decl;
    Type *type;
    Val val;
    const char *external_name;
} Sym;

typedef struct Package {
    const char *path;
    char full_path[MAX_PATH];
    Decl **decls;
    size_t num_decls;
    Map syms_map;
    Sym **syms;
    const char *external_name;
    bool always_reachable;
} Package;

enum {
    MAX_LOCAL_SYMS = 1024
};

Package *current_package;
Package *builtin_package;
Map package_map;
Package **package_list;

enum {
    REACHABLE_NONE,
    REACHABLE_NATURAL,
    REACHABLE_FORCED,
};

uint8_t reachable_phase = REACHABLE_NATURAL;

void set_resolved_sym(const void *ptr, Sym *sym);

Sym *get_package_sym(Package *package, const char *name) {
    return map_get(&package->syms_map, name);
}

void add_package(Package *package) {
    Package *old_package = map_get(&package_map, package->path);
    if (old_package != package) {
        assert(!old_package);
        map_put(&package_map, package->path, package);
        buf_push(package_list, package);
    }
}

Package *enter_package(Package *new_package) {
    Package *old_package = current_package;
    current_package = new_package;
    return old_package;
}

void leave_package(Package *old_package) {
    current_package = old_package;
}

Sym **reachable_syms;
Sym **sorted_syms;
Sym local_syms[MAX_LOCAL_SYMS];
Sym *local_syms_end = local_syms;

bool is_local_sym(Sym *sym) {
    return local_syms <= sym && sym < local_syms_end;
}

Sym *sym_new(SymKind kind, const char *name, Decl *decl) {
    Sym *sym = xcalloc(1, sizeof(Sym));
    sym->kind = kind;
    sym->name = name;
    sym->decl = decl;
    sym->package = current_package;
    set_resolved_sym(sym, sym);
    return sym;
}

void process_decl_notes(Decl *decl, Sym *sym) {
    Note *foreign_note = get_decl_note(decl, foreign_name);
    if (foreign_note) {
        if (foreign_note->num_args > 1) {
            fatal_error(decl->pos, "@foreign takes 0 or 1 argument");
        }
        const char *external_name;
        if (foreign_note->num_args == 0) {
            external_name = sym->name;
        } else {
            Expr *arg = foreign_note->args[0].expr;
            if (arg->kind != EXPR_STR) {
                fatal_error(decl->pos, "@foreign argument 1 must be a string literal");
            }
            external_name = arg->str_lit.val;
        }
        sym->external_name = external_name;
    }
}

Sym *sym_decl(Decl *decl) {
    SymKind kind = SYM_NONE;
    switch (decl->kind) {
    case DECL_STRUCT:
    case DECL_UNION:
    case DECL_TYPEDEF:
    case DECL_ENUM:
        kind = SYM_TYPE;
        break;
    case DECL_VAR:
        kind = SYM_VAR;
        break;
    case DECL_CONST:
        kind = SYM_CONST;
        break;
    case DECL_FUNC:
        kind = SYM_FUNC;
        break;
    default:
        assert(0);
        break;
    }
    Sym *sym = sym_new(kind, decl->name, decl);
    set_resolved_sym(decl, sym);
    process_decl_notes(decl, sym);
    return sym;
}

Sym *sym_get_local(const char *name) {
    for (Sym *it = local_syms_end; it != local_syms; it--) {
        Sym *sym = it-1;
        if (sym->name == name) {
            return sym;
        }
    }
    return NULL;
}

Sym *sym_get(const char *name) {
    Sym *sym = sym_get_local(name);
    return sym ? sym : get_package_sym(current_package, name);
}

bool sym_push_var(const char *name, Type *type) {
    if (sym_get_local(name)) {
        return false;
    }
    if (local_syms_end == local_syms + MAX_LOCAL_SYMS) {
        fatal("Too many local symbols");
    }
    *local_syms_end++ = (Sym){
        .name = name,
        .kind = SYM_VAR,
        .state = SYM_RESOLVED,
        .type = type,
    };
    return true;
}

Sym *sym_enter(void) {
    return local_syms_end;
}

void sym_leave(Sym *sym) {
    local_syms_end = sym;
}

void sym_global_put(const char *name, Sym *sym) {
    Sym *old_sym = map_get(&current_package->syms_map, name);
    if (old_sym) {
        if (sym == old_sym) {
            return;
        }
        SrcPos pos = sym->decl ? sym->decl->pos : pos_builtin;
        if (old_sym->decl) {
            warning(old_sym->decl->pos, "Previous definition of '%s'", name);
        }
        fatal_error(pos, "Duplicate definition of global symbol '%s'.", name);
    }
    map_put(&current_package->syms_map, name, sym);
    buf_push(current_package->syms, sym);
}

void resolve_sym(Sym *sym);

Sym *sym_global_type(const char *name, Type *type) {
    name = str_intern(name);
    Sym *sym = sym_new(SYM_TYPE, name, NULL);
    sym->state = SYM_RESOLVED;
    sym->type = type;
    sym->external_name = name;
    sym_global_put(name, sym);
    return sym;
}

Sym *sym_global_decl(Decl *decl) {
    Sym *sym = NULL;
    if (decl->name) {
        sym = sym_decl(decl);
        sym_global_put(sym->name, sym);
    }
    if (decl->kind == DECL_ENUM) {
        Typespec *enum_typespec = enum_typespec = new_typespec_name(decl->pos, sym ? sym->name : str_intern("int"));
        const char *prev_item_name = NULL;
        for (size_t i = 0; i < decl->enum_decl.num_items; i++) {
            EnumItem item = decl->enum_decl.items[i];
            Expr *init;
            if (item.init) {
                init = item.init;
            } else if (prev_item_name) {
                init = new_expr_binary(item.pos, TOKEN_ADD, new_expr_name(item.pos, prev_item_name), new_expr_int(item.pos, 1, 0, 0));
            } else {
                init = new_expr_int(item.pos, 0, 0, 0);
            }
            Decl *item_decl = new_decl_const(item.pos, item.name, enum_typespec, init);
            item_decl->notes = decl->notes;
            sym_global_decl(item_decl);
            prev_item_name = item.name;
        }
    }
    return sym;
}

void put_type_name(char **buf, Type *type) {
    const char *type_name = type_names[type->kind];
    if (type_name) {
        buf_printf(*buf, "%s", type_name);
    } else {
        switch (type->kind) {
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ENUM:
        case TYPE_INCOMPLETE:
            assert(type->sym);
            buf_printf(*buf, "%s", type->sym->name);
            break;
        case TYPE_CONST:
            put_type_name(buf, type->base);
            buf_printf(*buf, " const");
            break;
        case TYPE_PTR:
            put_type_name(buf, type->base);
            buf_printf(*buf, "*");
            break;
        case TYPE_ARRAY:
            put_type_name(buf, type->base);
            buf_printf(*buf, "[%llu]", type->num_elems);
            break;
        case TYPE_FUNC:
            buf_printf(*buf, "func(");
            for (size_t i = 0; i < type->func.num_params; i++) {
                if (i != 0) {
                    buf_printf(*buf, ", ");
                }
                put_type_name(buf, type->func.params[i]);
            }
            if (type->func.has_varargs) {
                buf_printf(*buf, "...");
            }
            buf_printf(*buf, ")");
            if (type->func.ret != type_void) {
                buf_printf(*buf, ": ");
                put_type_name(buf, type->func.ret);
            }
            break;
        default:
            assert(0);
            break;
        }
    }
}

char *get_type_name(Type *type) {
    char *buf = NULL;
    put_type_name(&buf, type);
    return buf;
}

typedef struct Operand {
    Type *type;
    bool is_lvalue;
    bool is_const;
    Val val;
} Operand;

Operand operand_null;

Operand operand_rvalue(Type *type) {
    return (Operand){
        .type = unqualify_type(type),
    };
}

Operand operand_lvalue(Type *type) {
    return (Operand){
        .type = type,
        .is_lvalue = true,
    };
}

Operand operand_const(Type *type, Val val) {
    return (Operand){
        .type = unqualify_type(type),
        .is_const = true,
        .val = val,
    };
}

Type *type_decay(Type *type) {
    type = unqualify_type(type);
    if (type->kind == TYPE_ARRAY) {
        type = type_ptr(type->base);
    }
    return type;
}

Operand operand_decay(Operand operand) {
    operand.type = type_decay(operand.type);
    operand.is_lvalue = false;
    return operand;
}

bool is_null_ptr(Operand operand);

#define CASE(k, t) \
    case k: \
        switch (type->kind) { \
        case TYPE_BOOL: \
            operand->val.b = (bool)operand->val.t; \
            break; \
        case TYPE_CHAR: \
            operand->val.c = (char)operand->val.t; \
            break; \
        case TYPE_UCHAR: \
            operand->val.uc = (unsigned char)operand->val.t; \
            break; \
        case TYPE_SCHAR: \
            operand->val.sc = (signed char)operand->val.t; \
            break; \
        case TYPE_SHORT: \
            operand->val.s = (short)operand->val.t; \
            break; \
        case TYPE_USHORT: \
            operand->val.us = (unsigned short)operand->val.t; \
            break; \
        case TYPE_INT: \
        case TYPE_ENUM: \
            operand->val.i = (int)operand->val.t; \
            break; \
        case TYPE_UINT: \
            operand->val.u = (unsigned)operand->val.t; \
            break; \
        case TYPE_LONG: \
            operand->val.l = (long)operand->val.t; \
            break; \
        case TYPE_ULONG: \
            operand->val.ul = (unsigned long)operand->val.t; \
            break; \
        case TYPE_LLONG: \
            operand->val.ll = (long long)operand->val.t; \
            break; \
        case TYPE_ULLONG: \
            operand->val.ull = (unsigned long long)operand->val.t; \
            break; \
        case TYPE_PTR: \
            operand->val.p = (uintptr_t)operand->val.t; \
            break; \
        case TYPE_FLOAT: \
        case TYPE_DOUBLE: \
            break; \
        default: \
            operand->is_const = false; \
            break; \
        } \
        break;

bool is_convertible(Operand *operand, Type *dest) {
    dest = unqualify_type(dest);
    Type *src = unqualify_type(operand->type);
    if (dest == src) {
        return true;
    } else if (is_arithmetic_type(dest) && is_arithmetic_type(src)) {
        return true;
    } else if (is_ptr_like_type(dest) && is_null_ptr(*operand)) {
        return true;
    } else if (is_ptr_type(dest) && is_ptr_type(src)) {
        if (is_const_type(dest->base) && is_const_type(src->base)) {
            return dest->base->base == src->base->base || dest->base->base == type_void || src->base->base == type_void;
        } else {
            Type *unqual_dest_base = unqualify_type(dest->base);
            if (unqual_dest_base == src->base) {
                return true;
            } else if (unqual_dest_base == type_void) {
                return is_const_type(dest->base) || !is_const_type(src->base);
            } else {
                return src->base == type_void;
            }
        }
    } else {
        return false;
    }
}

bool is_castable(Operand *operand, Type *dest) {
    Type *src = operand->type;
    if (is_convertible(operand, dest)) {
        return true;
    } else if (is_integer_type(dest)) {
        return is_ptr_like_type(src);
    } else if (is_integer_type(src)) {
        return is_ptr_like_type(dest);
    } else if (is_ptr_like_type(dest) && is_ptr_like_type(src)) {
        return true;
    } else {
        return false;
    }
}

bool cast_operand(Operand *operand, Type *type) {
    Type *qual_type = type;
    type = unqualify_type(type);
    operand->type = unqualify_type(operand->type);
    if (operand->type != type) {
        if (!is_castable(operand, type)) {
            return false;
        }
        if (operand->is_const) {
            if (is_floating_type(operand->type)) {
                operand->is_const = !is_integer_type(type);
            } else {
                if (type->kind == TYPE_ENUM) {
                    type = type->base;
                }
                Type *operand_type = operand->type;
                if (operand_type->kind == TYPE_ENUM) {
                    operand_type = operand_type->base;
                }
                switch (operand_type->kind) {
                CASE(TYPE_BOOL, b)
                CASE(TYPE_CHAR, c)
                CASE(TYPE_UCHAR, uc)
                CASE(TYPE_SCHAR, sc)
                CASE(TYPE_SHORT, s)
                CASE(TYPE_USHORT, us)
                CASE(TYPE_INT, i)
                CASE(TYPE_UINT, u)
                CASE(TYPE_LONG, l)
                CASE(TYPE_ULONG, ul)
                CASE(TYPE_LLONG, ll)
                CASE(TYPE_ULLONG, ull)
                CASE(TYPE_PTR, p)
                default:
                    operand->is_const = false;
                    break;
                }
            }
        }
    }
    operand->type = qual_type;
    return true;
}

bool convert_operand(Operand *operand, Type *type) {
    if (is_convertible(operand, type)) {
        cast_operand(operand, type);
        operand->type = unqualify_type(operand->type);
        operand->is_lvalue = false;
        return true;
    }
    return false;
}

#undef CASE

bool is_null_ptr(Operand operand) {
    if (operand.is_const && (is_ptr_type(operand.type) || is_integer_type(operand.type))) {
        cast_operand(&operand, type_ullong);
        return operand.val.ull == 0;
    } else {
        return false;
    }
}

void promote_operand(Operand *operand) {
    switch (operand->type->kind) {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_SCHAR:
    case TYPE_UCHAR:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_ENUM:
        cast_operand(operand, type_int);
        break;
    default:
        // Do nothing
        break;
    }
}

void unify_arithmetic_operands(Operand *left, Operand *right) {
    if (left->type == type_double) {
        cast_operand(right, type_double);
    } else if (right->type == type_double) {
        cast_operand(left, type_double);
    } else if (left->type == type_float) {
        cast_operand(right, type_float);
    } else if (right->type == type_float) {
        cast_operand(left, type_float);
    } else {
        assert(is_integer_type(left->type));
        assert(is_integer_type(right->type));
        promote_operand(left);
        promote_operand(right);
        if (left->type != right->type) {
            if (is_signed_type(left->type) == is_signed_type(right->type)) {
                if (type_rank(left->type) <= type_rank(right->type)) {
                    cast_operand(left, right->type);
                } else {
                    cast_operand(right, left->type);
                }
            } else if (is_signed_type(left->type) && type_rank(right->type) >= type_rank(left->type)) {
                cast_operand(left, right->type);
            } else if (is_signed_type(right->type) && type_rank(left->type) >= type_rank(right->type)) {
                cast_operand(right, left->type);
            } else if (is_signed_type(left->type) && type_sizeof(left->type) > type_sizeof(right->type)) {
                cast_operand(right, left->type);            
            } else if (is_signed_type(right->type) && type_sizeof(right->type) > type_sizeof(left->type)) {
                cast_operand(left, right->type);
            } else { 
                Type *type = unsigned_type(is_signed_type(left->type) ? left->type : right->type);
                cast_operand(left, type);
                cast_operand(right, type);
            }
        }
    }
    assert(left->type == right->type);
}

Map resolved_type_map;

Type *get_resolved_type(void *ptr) {
    return map_get(&resolved_type_map, ptr);
}

void set_resolved_type(void *ptr, Type *type) {
    map_put(&resolved_type_map, ptr, type);
}

Map resolved_sym_map;

Sym *get_resolved_sym(const void *ptr) {
    return map_get(&resolved_sym_map, ptr);
}

void set_resolved_sym(const void *ptr, Sym *sym) {
    if (!is_local_sym(sym)) {
        map_put(&resolved_sym_map, ptr, sym);
    }
}

Map resolved_expected_type_map;

Type *get_resolved_expected_type(Expr *expr) {
    return map_get(&resolved_expected_type_map, expr);
}

void set_resolved_expected_type(Expr *expr, Type *type) {
    if (expr && type) {
        map_put(&resolved_expected_type_map, expr, type);
    }
}

Sym *resolve_name(const char *name);
Operand resolve_const_expr(Expr *expr);
Operand resolve_expected_expr(Expr *expr, Type *expected_type);

Operand resolve_expr(Expr *expr) {
    return resolve_expected_expr(expr, NULL);
}

Operand resolve_expr_rvalue(Expr *expr) {
    return operand_decay(resolve_expr(expr));
}

Operand resolve_expected_expr_rvalue(Expr *expr, Type *expected_type) {
    return operand_decay(resolve_expected_expr(expr, expected_type));
}

Type *resolve_typespec(Typespec *typespec) {
    if (!typespec) {
        return type_void;
    }
    Type *result = NULL;
    switch (typespec->kind) {
    case TYPESPEC_NAME: {
        Sym *sym = resolve_name(typespec->name);
        if (!sym) {
            fatal_error(typespec->pos, "Unresolved type name '%s'", typespec->name);
        }
        if (sym->kind != SYM_TYPE) {
            fatal_error(typespec->pos, "%s must denote a type", typespec->name);
            return NULL;
        }
        set_resolved_sym(typespec, sym);
        result = sym->type;
        break;
    }
    case TYPESPEC_CONST:
        result = type_const(resolve_typespec(typespec->base));
        break;
    case TYPESPEC_PTR:
        result = type_ptr(resolve_typespec(typespec->base));
        break;
    case TYPESPEC_ARRAY: {
        int size = 0;
        if (typespec->num_elems) {
            Operand operand = resolve_const_expr(typespec->num_elems);
            if (!is_integer_type(operand.type)) {
                fatal_error(typespec->pos, "Array size constant expression must have integer type");
            }
            cast_operand(&operand, type_int);
            size = operand.val.i;
            if (size <= 0) {
                fatal_error(typespec->num_elems->pos, "Non-positive array size");
            }
        }
        result = type_array(resolve_typespec(typespec->base), size);
        break;
    }
    case TYPESPEC_FUNC: {
        Type **args = NULL;
        for (size_t i = 0; i < typespec->func.num_args; i++) {
            Type *arg = resolve_typespec(typespec->func.args[i]);
            if (arg == type_void) {
                fatal_error(typespec->pos, "Function parameter type cannot be void");
            }
            buf_push(args, arg);
        }
        Type *ret = type_void;
        if (typespec->func.ret) {
            ret = resolve_typespec(typespec->func.ret);
        }
        if (is_array_type(ret)) {
            fatal_error(typespec->pos, "Function return type cannot be array");
        }
        result = type_func(args, buf_len(args), ret, false);
        break;
    }
    default:
        assert(0);
        return NULL;
    }
    set_resolved_type(typespec, result);
    return result;
}

void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal_error(type->sym->decl->pos, "Type completion cycle");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }
    Sym *sym = type->sym;
    Package *old_package = enter_package(sym->package);
    Decl *decl = sym->decl;
    if (decl->is_incomplete) {
        fatal_error(decl->pos, "Trying to use incomplete type as complete type");
    }
    type->kind = TYPE_COMPLETING;
    assert(decl->kind == DECL_STRUCT || decl->kind == DECL_UNION);
    TypeField *fields = NULL;
    for (size_t i = 0; i < decl->aggregate.num_items; i++) {
        AggregateItem item = decl->aggregate.items[i];
        Type *item_type = resolve_typespec(item.type);
        complete_type(item_type);
        if (type_sizeof(item_type) == 0) {
            fatal_error(item.pos, "Field type of size 0 is not allowed");
        }
        for (size_t j = 0; j < item.num_names; j++) {
            buf_push(fields, (TypeField){item.names[j], item_type});
        }
    }
    if (buf_len(fields) == 0) {
        fatal_error(decl->pos, "No fields");
    }
    if (has_duplicate_fields(fields, buf_len(fields))) {
        fatal_error(decl->pos, "Duplicate fields");
    }
    if (decl->kind == DECL_STRUCT) {
        type_complete_struct(type, fields, buf_len(fields));
    } else {
        assert(decl->kind == DECL_UNION);
        type_complete_union(type, fields, buf_len(fields));
    }
    buf_push(sorted_syms, type->sym);
    leave_package(old_package);
}

Type *resolve_typed_init(SrcPos pos, Type *type, Expr *expr) {
    Type *expected_type = unqualify_type(type);
    Operand operand = resolve_expected_expr(expr, expected_type);
    if (is_incomplete_array_type(type) && is_array_type(operand.type) && type->base == operand.type->base) {
        // Incomplete array size, so infer the size from the initializer expression's type.
    } else {
        if (type && is_ptr_type(type)) {
            operand = operand_decay(operand);
        }
        if (!convert_operand(&operand, expected_type)) {
            return NULL;
        }
    }
    set_resolved_expected_type(expr, operand.type);
    return operand.type;
}

Type *resolve_init(SrcPos pos, Typespec *typespec, Expr *expr) {
    Type *type;
    if (typespec) {
        Type *declared_type = resolve_typespec(typespec);
        type = declared_type;
        if (expr) {
            type = resolve_typed_init(pos, declared_type, expr);
            if (!type) {
                fatal_error(pos, "Invalid type in initialization. Expected %s", get_type_name(declared_type));
            }
        }
    } else {
        assert(expr);
        type = unqualify_type(resolve_expr(expr).type);
        if (is_array_type(type) && expr->kind != EXPR_COMPOUND) {
            type = type_decay(type);
            set_resolved_type(expr, type);
        }
        set_resolved_expected_type(expr, type);
    }
    complete_type(type);
    if (type->size == 0) {
        fatal_error(pos, "Cannot declare variable of size 0");
    }
    return type;
}

Type *resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);
    return resolve_init(decl->pos, decl->var.type, decl->var.expr);
}

Type *resolve_decl_const(Decl *decl, Val *val) {
    assert(decl->kind == DECL_CONST);
    Operand result = resolve_const_expr(decl->const_decl.expr);
    if (!is_scalar_type(result.type)) {
        fatal_error(decl->pos, "Const declarations must have scalar type");
    }
    if (decl->const_decl.type) {
        Type *type = resolve_typespec(decl->const_decl.type);
        if (!convert_operand(&result, type)) {
            fatal_error(decl->pos, "Invalid type in constant declaration. Expected %s, got %s", get_type_name(type), get_type_name(result.type));
        }
    }
    *val = result.val;
    return result.type;
}

Type *resolve_decl_func(Decl *decl) {
    assert(decl->kind == DECL_FUNC);
    Type **params = NULL;
    for (size_t i = 0; i < decl->func.num_params; i++) {
        Type *param = resolve_typespec(decl->func.params[i].type);
        complete_type(param);
        if (param == type_void) {
            fatal_error(decl->pos, "Function parameter type cannot be void");
        }
        buf_push(params, param);
    }
    Type *ret_type = type_void;
    if (decl->func.ret_type) {
        ret_type = resolve_typespec(decl->func.ret_type);
        complete_type(ret_type);
    }
    if (is_array_type(ret_type)) {
        fatal_error(decl->pos, "Function return type cannot be array");
    }
    return type_func(params, buf_len(params), ret_type, decl->func.has_varargs);
}

typedef struct StmtCtx {
    bool is_break_legal;
    bool is_continue_legal;
} StmtCtx;

bool resolve_stmt(Stmt *stmt, Type *ret_type, StmtCtx ctx);

bool is_cond_operand(Operand operand) {
    operand = operand_decay(operand);
    return is_scalar_type(operand.type);
}

void resolve_cond_expr(Expr *expr) {
    Operand cond = resolve_expr_rvalue(expr);
    if (!is_cond_operand(cond)) {
        fatal_error(expr->pos, "Conditional expression must have scalar type");
    }
}

bool resolve_stmt_block(StmtList block, Type *ret_type, StmtCtx ctx) {
    Sym *scope = sym_enter();
    bool returns = false;
    for (size_t i = 0; i < block.num_stmts; i++) {
        returns = resolve_stmt(block.stmts[i], ret_type, ctx) || returns;
    }
    sym_leave(scope);
    return returns;
}

Operand resolve_expr_binary_op(TokenKind op, const char *op_name, SrcPos pos, Operand left, Operand right);
Operand resolve_name_operand(SrcPos pos, const char *name);

void resolve_stmt_assign(Stmt *stmt) {
    assert(stmt->kind == STMT_ASSIGN);
    Operand left = resolve_expr(stmt->assign.left);
    if (!left.is_lvalue) {
        fatal_error(stmt->pos, "Cannot assign to non-lvalue");
    }
    if (is_array_type(left.type)) {
        fatal_error(stmt->pos, "Cannot assign to array");
    }
    if (left.type->nonmodifiable) {
        fatal_error(stmt->pos, "Left-hand side of assignment has non-modifiable type");
    }
    const char *assign_op_name = token_kind_name(stmt->assign.op);
    TokenKind binary_op = assign_token_to_binary_token[stmt->assign.op];
    Operand right = resolve_expected_expr_rvalue(stmt->assign.right, left.type);
    Operand result;
    if (stmt->assign.op == TOKEN_ASSIGN) {
        result = right;
    } else if (stmt->assign.op == TOKEN_ADD_ASSIGN || stmt->assign.op == TOKEN_SUB_ASSIGN) {
        if (left.type->kind == TYPE_PTR && is_integer_type(right.type)) {
            result = operand_rvalue(left.type);
        } else if (is_arithmetic_type(left.type) && is_arithmetic_type(right.type)) {
            result = resolve_expr_binary_op(binary_op, assign_op_name, stmt->pos, left, right);
        } else {
            fatal_error(stmt->pos, "Invalid operand types for %s", assign_op_name);
        }
    } else {
        result = resolve_expr_binary_op(binary_op, assign_op_name, stmt->pos, left, right);
    }
    if (!convert_operand(&result, left.type)) {
        fatal_error(stmt->pos, "Invalid type in assignment. Expected %s, got %s", get_type_name(left.type), get_type_name(result.type));
    }
}

void resolve_stmt_init(Stmt *stmt) {
    assert(stmt->kind == STMT_INIT);
    Type *type = resolve_init(stmt->pos, stmt->init.type, stmt->init.expr);
    if (!sym_push_var(stmt->init.name, type)) {
        fatal_error(stmt->pos, "Shadowed definition of local symbol");
    }
}

void resolve_static_assert(Note note) {
    if (note.num_args != 1) {
        fatal_error(note.pos, "#static_assert takes 1 argument");
    }
    Operand operand = resolve_const_expr(note.args[0].expr);
    if (!operand.val.ull) {
        fatal_error(note.pos, "#static_assert failed");
    }
}

bool resolve_stmt(Stmt *stmt, Type *ret_type, StmtCtx ctx) {
    switch (stmt->kind) {
    case STMT_RETURN:
        if (stmt->expr) {
            Operand operand = resolve_expected_expr(stmt->expr, ret_type);
            if (!convert_operand(&operand, ret_type)) {
                fatal_error(stmt->pos, "Invalid type in return expression. Expected %s, got %s", get_type_name(ret_type), get_type_name(operand.type));
            }
        } else if (ret_type != type_void) {
            fatal_error(stmt->pos, "Empty return expression for function with non-void return type");
        }
        return true;
    case STMT_BREAK:
        if (!ctx.is_break_legal) {
            fatal_error(stmt->pos, "Illegal break");
        }
        return false;
    case STMT_CONTINUE:
        if (!ctx.is_continue_legal) {
            fatal_error(stmt->pos, "Illegal continue");
        }
        return false;
    case STMT_BLOCK:
        return resolve_stmt_block(stmt->block, ret_type, ctx);
    case STMT_NOTE:
        if (stmt->note.name == assert_name) {
            if (stmt->note.num_args != 1) {
                fatal_error(stmt->pos, "#assert takes 1 argument");
            }
            resolve_cond_expr(stmt->note.args[0].expr);
        } else if (stmt->note.name == static_assert_name) {
            resolve_static_assert(stmt->note);
        } else {
            warning(stmt->pos, "Unknown statement #directive '%s'", stmt->note.name);
        }
        return false;
    case STMT_IF: {
        Sym *scope = sym_enter();
        if (stmt->if_stmt.init) {
            resolve_stmt_init(stmt->if_stmt.init);
        }
        if (stmt->if_stmt.cond) {
            resolve_cond_expr(stmt->if_stmt.cond);
        } else if (!is_cond_operand(resolve_name_operand(stmt->pos, stmt->if_stmt.init->init.name))) {
            fatal_error(stmt->pos, "Conditional expression must have scalar type");
        }
        bool returns = resolve_stmt_block(stmt->if_stmt.then_block, ret_type, ctx);
        for (size_t i = 0; i < stmt->if_stmt.num_elseifs; i++) {
            ElseIf elseif = stmt->if_stmt.elseifs[i];
            resolve_cond_expr(elseif.cond);
            returns = resolve_stmt_block(elseif.block, ret_type, ctx) && returns;
        }
        if (stmt->if_stmt.else_block.stmts) {
            returns = resolve_stmt_block(stmt->if_stmt.else_block, ret_type, ctx) && returns;
        } else {
            returns = false;
        }
        sym_leave(scope);
        return returns;
    }
    case STMT_WHILE:
    case STMT_DO_WHILE:
        resolve_cond_expr(stmt->while_stmt.cond);
        ctx.is_break_legal = true;
        ctx.is_continue_legal = true;
        resolve_stmt_block(stmt->while_stmt.block, ret_type, ctx);
        return false;
    case STMT_FOR: {
        Sym *scope = sym_enter();
        if (stmt->for_stmt.init) {
            resolve_stmt(stmt->for_stmt.init, ret_type, ctx);
        }
        if (stmt->for_stmt.cond) {
            resolve_cond_expr(stmt->for_stmt.cond);
        }
        if (stmt->for_stmt.next) {
            resolve_stmt(stmt->for_stmt.next, ret_type, ctx);
        }
        ctx.is_break_legal = true;
        ctx.is_continue_legal = true;
        resolve_stmt_block(stmt->for_stmt.block, ret_type, ctx);
        sym_leave(scope);
        return false;
    }
    case STMT_SWITCH: {
        Operand operand = resolve_expr_rvalue(stmt->switch_stmt.expr);
        if (!is_integer_type(operand.type)) {
            fatal_error(stmt->pos, "Switch expression must have integer type");
        }
        ctx.is_break_legal = true;
        bool returns = true;
        bool has_default = false;
        for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
            SwitchCase switch_case = stmt->switch_stmt.cases[i];
            for (size_t j = 0; j < switch_case.num_exprs; j++) {
                Expr *case_expr = switch_case.exprs[j];
                Operand case_operand = resolve_expr(case_expr);
                if (!convert_operand(&case_operand, operand.type)) {
                    fatal_error(case_expr->pos, "Invalid type in switch case expression. Expected %s, got %s", get_type_name(operand.type), get_type_name(case_operand.type));
                }
            }
            if (switch_case.is_default) {
                if (has_default) {
                    fatal_error(stmt->pos, "Switch statement has multiple default clauses");
                }
                has_default = true;
            }
            if (switch_case.block.num_stmts > 1) {
                Stmt *last_stmt = switch_case.block.stmts[switch_case.block.num_stmts - 1];
                if (last_stmt->kind == STMT_BREAK) {
                    warning(last_stmt->pos, "Case blocks already end with an implicit break");
                }
            }
            returns = resolve_stmt_block(switch_case.block, ret_type, ctx) && returns;
        }
        return returns && has_default;
    }
    case STMT_ASSIGN:
        resolve_stmt_assign(stmt);
        return false;
    case STMT_INIT:
        resolve_stmt_init(stmt);
        return false;
    case STMT_EXPR:
        resolve_expr(stmt->expr);
        return false;
    default:
        assert(0);
        return false;
    }
}

void resolve_func_body(Sym *sym) {
    Decl *decl = sym->decl;
    assert(decl->kind == DECL_FUNC);
    assert(sym->state == SYM_RESOLVED);
    if (decl->is_incomplete) {
        return;
    }
    Package *old_package = enter_package(sym->package);
    Sym *scope = sym_enter();
    for (size_t i = 0; i < decl->func.num_params; i++) {
        FuncParam param = decl->func.params[i];
        Type *param_type = resolve_typespec(param.type);
        if (is_array_type(param_type)) {
            param_type = type_ptr(param_type->base);
        }
        sym_push_var(param.name, param_type);
    }
    Type *ret_type = resolve_typespec(decl->func.ret_type);
    assert(!is_array_type(ret_type));
    bool returns = resolve_stmt_block(decl->func.block, ret_type, (StmtCtx){0});
    sym_leave(scope);
    if (ret_type != type_void && !returns) {
        fatal_error(decl->pos, "Not all control paths return values");
    }
    leave_package(old_package);
}


void resolve_sym(Sym *sym) {
    if (sym->state == SYM_RESOLVED) {
        return;
    } else if (sym->state == SYM_RESOLVING) {
        fatal_error(sym->decl->pos, "Cyclic dependency");
        return;
    }
    assert(sym->state == SYM_UNRESOLVED);
    assert(!sym->reachable);
    if (!is_local_sym(sym)) {
        buf_push(reachable_syms, sym);
        sym->reachable = reachable_phase;
    }
    sym->state = SYM_RESOLVING;
    Decl *decl = sym->decl;
    Package *old_package = enter_package(sym->package);
    switch (sym->kind) {
    case SYM_TYPE:
        if (decl && decl->kind == DECL_TYPEDEF) {
            sym->type = resolve_typespec(decl->typedef_decl.type);
        } else if (decl->kind == DECL_ENUM) {
            Type *base = decl->enum_decl.type ? resolve_typespec(decl->enum_decl.type) : type_int;
            if (!is_integer_type(base)) {
                fatal_error(decl->pos, "Base type of enum must be integer type");
            }
            sym->type = type_enum(sym, base);
        } else {
            sym->type = type_incomplete(sym);
        }
        break;
    case SYM_VAR:
        sym->type = resolve_decl_var(decl);
        break;
    case SYM_CONST:
        sym->type = resolve_decl_const(decl, &sym->val);
        break;
    case SYM_FUNC:
        sym->type = resolve_decl_func(decl);
        break;
    default:
        assert(0);
        break;
    }
    leave_package(old_package);
    sym->state = SYM_RESOLVED;
    if (decl->is_incomplete || (decl->kind != DECL_STRUCT && decl->kind != DECL_UNION)) {
        buf_push(sorted_syms, sym);
    }
}

void finalize_sym(Sym *sym) {
    assert(sym->state == SYM_RESOLVED);
    if (sym->decl && !is_decl_foreign(sym->decl) && !sym->decl->is_incomplete) {
        if (sym->kind == SYM_TYPE) {
            complete_type(sym->type);
        } else if (sym->kind == SYM_FUNC) {
            resolve_func_body(sym);
        }
    }
}

Sym *resolve_name(const char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        return NULL;
    }
    resolve_sym(sym);
    return sym;
}

Operand resolve_expr_field(Expr *expr) {
    assert(expr->kind == EXPR_FIELD);
    Operand operand = resolve_expr(expr->field.expr);
    bool was_const_type = is_const_type(operand.type);
    Type *type = unqualify_type(operand.type);
    complete_type(type);
    if (is_ptr_type(type)) {
        operand = operand_lvalue(type->base);
        was_const_type = is_const_type(operand.type);
        type = unqualify_type(operand.type);
        complete_type(type);
    }
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
        fatal_error(expr->pos, "Can only access fields on aggregates or pointers to aggregates");
        return operand_null;
    }
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        TypeField field = type->aggregate.fields[i];
        if (field.name == expr->field.name) {
            Operand field_operand = operand.is_lvalue ? operand_lvalue(field.type) : operand_rvalue(field.type);
            if (was_const_type) {
                field_operand.type = type_const(field_operand.type);
            }
            return field_operand;
        }
    }
    fatal_error(expr->pos, "No field named '%s'", expr->field.name);
    return operand_null;
}

long long eval_unary_op_ll(TokenKind op, long long val) {
    switch (op) {
    case TOKEN_ADD:
        return +val;
    case TOKEN_SUB:
        return -val;
    case TOKEN_NEG:
        return ~val;
    case TOKEN_NOT:
        return !val;
    default:
        assert(0);
        break;
    }
    return 0;
}

unsigned long long eval_unary_op_ull(TokenKind op, unsigned long long val) {
    switch (op) {
    case TOKEN_ADD:
        return +val;
    case TOKEN_SUB:
        return 0ull - val;
    case TOKEN_NEG:
        return ~val;
    case TOKEN_NOT:
        return !val;
    default:
        assert(0);
        break;
    }
    return 0;
}

long long eval_binary_op_ll(TokenKind op, long long left, long long right) {
    switch (op) {
    case TOKEN_MUL:
        return left * right;
    case TOKEN_DIV:
        return right != 0 ? left / right : 0;
    case TOKEN_MOD:
        return right != 0 ? left % right : 0;
    case TOKEN_AND:
        return left & right;
    case TOKEN_LSHIFT:
        return left << right;
    case TOKEN_RSHIFT:
        return left >> right;
    case TOKEN_ADD:
        return left + right;
    case TOKEN_SUB:
        return left - right;
    case TOKEN_OR:
        return left | right;
    case TOKEN_XOR:
        return left ^ right;
    case TOKEN_EQ:
        return left == right;
    case TOKEN_NOTEQ:
        return left != right;
    case TOKEN_LT:
        return left < right;
    case TOKEN_LTEQ:
        return left <= right;
    case TOKEN_GT:
        return left > right;
    case TOKEN_GTEQ:
        return left >= right;
    default:
        assert(0);
        break;
    }
    return 0;
}

unsigned long long eval_binary_op_ull(TokenKind op, unsigned long long left, unsigned long long right) {
    switch (op) {
    case TOKEN_MUL:
        return left * right;
    case TOKEN_DIV:
        return right != 0 ? left / right : 0;
    case TOKEN_MOD:
        return right != 0 ? left % right : 0;
    case TOKEN_AND:
        return left & right;
    case TOKEN_LSHIFT:
        return left << right;
    case TOKEN_RSHIFT:
        return left >> right;
    case TOKEN_ADD:
        return left + right;
    case TOKEN_SUB:
        return left - right;
    case TOKEN_OR:
        return left | right;
    case TOKEN_XOR:
        return left ^ right;
    case TOKEN_EQ:
        return left == right;
    case TOKEN_NOTEQ:
        return left != right;
    case TOKEN_LT:
        return left < right;
    case TOKEN_LTEQ:
        return left <= right;
    case TOKEN_GT:
        return left > right;
    case TOKEN_GTEQ:
        return left >= right;
    default:
        assert(0);
        break;
    }
    return 0;
}

Val eval_unary_op(TokenKind op, Type *type, Val val) {
    if (is_integer_type(type)) {
        Operand operand = operand_const(type, val);
        if (is_signed_type(type)) {
            cast_operand(&operand, type_llong);
            operand.val.ll = eval_unary_op_ll(op, operand.val.ll);
        } else {
            cast_operand(&operand, type_ullong);
            operand.val.ll = eval_unary_op_ull(op, operand.val.ull);
        }
        cast_operand(&operand, type);
        return operand.val;
    } else {
        return (Val){0};
    }
}

Val eval_binary_op(TokenKind op, Type *type, Val left, Val right) {
    if (is_integer_type(type)) {
        Operand left_operand = operand_const(type, left);
        Operand right_operand = operand_const(type, right);
        Operand result_operand;
        if (is_signed_type(type)) {
            cast_operand(&left_operand, type_llong);
            cast_operand(&right_operand, type_llong);
            result_operand = operand_const(type_llong, (Val){.ll = eval_binary_op_ll(op, left_operand.val.ll, right_operand.val.ll)});
        } else {
            cast_operand(&left_operand, type_ullong);
            cast_operand(&right_operand, type_ullong);
            result_operand = operand_const(type_ullong, (Val){.ull = eval_binary_op_ull(op, left_operand.val.ull, right_operand.val.ull)});
        }
        cast_operand(&result_operand, type);
        return result_operand.val;
    } else {
        return (Val){0};
    }
}

Operand resolve_name_operand(SrcPos pos, const char *name) {
    Sym *sym = resolve_name(name);
    if (!sym) {
        fatal_error(pos, "Unresolved name '%s'", name);
    }
    if (sym->kind == SYM_VAR) {
        Operand operand = operand_lvalue(sym->type);
        if (is_array_type(operand.type)) {
            operand = operand_decay(operand);
        }
        return operand;
    } else if (sym->kind == SYM_CONST) {
        return operand_const(sym->type, sym->val);
    } else if (sym->kind == SYM_FUNC) {
        return operand_rvalue(sym->type);
    } else {
        fatal_error(pos, "%s must be a var or const", name);
        return operand_null;
    }
}

Operand resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    return resolve_name_operand(expr->pos, expr->name);
}

Operand resolve_unary_op(TokenKind op, Operand operand) {
    promote_operand(&operand);
    if (operand.is_const) {
        return operand_const(operand.type, eval_unary_op(op, operand.type, operand.val));
    } else {
        return operand;
    }
}

Operand resolve_expr_unary(Expr *expr) {
    Operand operand = resolve_expr_rvalue(expr->unary.expr);
    Type *type = operand.type;
    switch (expr->unary.op) {
    case TOKEN_MUL:
        if (!is_ptr_type(type)) {
            fatal_error(expr->pos, "Cannot deref non-ptr type");
        }
        return operand_lvalue(type->base);
    case TOKEN_ADD:
    case TOKEN_SUB:
        if (!is_arithmetic_type(type)) {
            fatal_error(expr->pos, "Can only use unary %s with arithmetic types", token_kind_name(expr->unary.op));
        }
        return resolve_unary_op(expr->unary.op, operand);
    case TOKEN_NEG:
        if (!is_integer_type(type)) {
            fatal_error(expr->pos, "Can only use ~ with integer types");
        }
        return resolve_unary_op(expr->unary.op, operand);
    case TOKEN_NOT:
        if (!is_scalar_type(type)) {
            fatal_error(expr->pos," Can only use ! with scalar types");
        }
        return resolve_unary_op(expr->unary.op, operand);
    default:
        assert(0);
        break;
    }
    return (Operand){0};
}

Operand resolve_binary_op(TokenKind op, Operand left, Operand right) {
    if (left.is_const && right.is_const) {
        return operand_const(left.type, eval_binary_op(op, left.type, left.val, right.val));
    } else {
        return operand_rvalue(left.type);
    }
}

Operand resolve_binary_arithmetic_op(TokenKind op, Operand left, Operand right) {
    unify_arithmetic_operands(&left, &right);
    return resolve_binary_op(op, left, right);
}

Operand resolve_expr_binary_op(TokenKind op, const char *op_name, SrcPos pos, Operand left, Operand right) {
    switch (op) {
    case TOKEN_MUL:
    case TOKEN_DIV:
        if (!is_arithmetic_type(left.type)) {
            fatal_error(pos, "Left operand of %s must have arithmetic type", op_name);
        }
        if (!is_arithmetic_type(right.type)) {
            fatal_error(pos, "Right operand of %s must have arithmetic type", op_name);
        }
        return resolve_binary_arithmetic_op(op, left, right);
    case TOKEN_MOD:
        if (!is_integer_type(left.type)) {
            fatal_error(pos, "Left operand of %% must have integer type");
        }
        if (!is_integer_type(right.type)) {
            fatal_error(pos, "Right operand of %% must have integer type");
        }
        return resolve_binary_arithmetic_op(op, left, right);
    case TOKEN_ADD:
        if (is_arithmetic_type(left.type) && is_arithmetic_type(right.type)) {
            return resolve_binary_arithmetic_op(op, left, right);
        } else if (is_ptr_type(left.type) && is_integer_type(right.type)) {
            return operand_rvalue(left.type);
        } else if (is_ptr_type(right.type) && is_integer_type(left.type)) {
            return operand_rvalue(right.type);
        } else {
            fatal_error(pos, "Operands of + must both have arithmetic type, or pointer and integer type");
        }
        break;
    case TOKEN_SUB:
        if (is_arithmetic_type(left.type) && is_arithmetic_type(right.type)) {
            return resolve_binary_arithmetic_op(op, left, right);
        } else if (is_ptr_type(left.type) && is_integer_type(right.type)) {
            return operand_rvalue(left.type);
        } else if (is_ptr_type(left.type) && is_ptr_type(right.type)) {
            if (left.type->base != right.type->base) {
                fatal_error(pos, "Cannot subtract pointers to different types");
            }
            return operand_rvalue(type_ssize);
        } else {
            fatal_error(pos, "Operands of - must both have arithmetic type, pointer and integer type, or compatible pointer types");
        }
        break;
    case TOKEN_LSHIFT:
    case TOKEN_RSHIFT:
        if (is_integer_type(left.type) && is_integer_type(right.type)) {
            promote_operand(&left);
            promote_operand(&right);
            Type *result_type = left.type;
            Operand result;
            if (is_signed_type(left.type)) {
                cast_operand(&left, type_llong);
                cast_operand(&right, type_llong);
            } else {
                cast_operand(&left, type_ullong);
                cast_operand(&right, type_ullong);
            }
            result = resolve_binary_op(op, left, right);
            cast_operand(&result, result_type);
            return result;
        } else {
            fatal_error(pos, "Operands of %s must both have integer type", op_name);
        }
        break;
    case TOKEN_LT:
    case TOKEN_LTEQ:
    case TOKEN_GT:
    case TOKEN_GTEQ:
    case TOKEN_EQ:
    case TOKEN_NOTEQ:
        if (is_arithmetic_type(left.type) && is_arithmetic_type(right.type)) {
            Operand result = resolve_binary_arithmetic_op(op, left, right);
            cast_operand(&result, type_int);
            return result;
        } else if (is_ptr_type(left.type) && is_ptr_type(right.type)) {
            if (unqualify_type(left.type->base) != unqualify_type(right.type->base)) {
                fatal_error(pos, "Cannot compare pointers to different types");
            }
            return operand_rvalue(type_int);
        } else if ((is_null_ptr(left) && is_ptr_type(right.type)) || (is_null_ptr(right) && is_ptr_type(left.type))) {
            return operand_rvalue(type_int);
        } else {
            fatal_error(pos, "Operands of %s must be arithmetic types or compatible pointer types", op_name);
        }
        break;
    case TOKEN_AND:
    case TOKEN_XOR:
    case TOKEN_OR:
        if (is_integer_type(left.type) && is_integer_type(right.type)) {
            return resolve_binary_arithmetic_op(op, left, right);
        } else {
            fatal_error(pos, "Operands of %s must have arithmetic types", op_name);
        }
        break;
    case TOKEN_AND_AND:
    case TOKEN_OR_OR:
        if (is_scalar_type(left.type) && is_scalar_type(right.type)) {
            if (left.is_const && right.is_const) {
                cast_operand(&left, type_bool);
                cast_operand(&right, type_bool);
                int i;
                if (op == TOKEN_AND_AND) {
                    i = left.val.b && right.val.b;
                } else {
                    assert(op == TOKEN_OR_OR);
                    i = left.val.b || right.val.b;
                }
                return operand_const(type_int, (Val){.i = i});
            } else {
                return operand_rvalue(type_int);
            }
        } else {
            fatal_error(pos, "Operands of %s must have scalar types", op_name);
        }
        break;
    default:
        assert(0);
        break;
    }
    return (Operand){0};
}

Operand resolve_expr_binary(Expr *expr) {
    assert(expr->kind == EXPR_BINARY);
    Operand left = resolve_expr_rvalue(expr->binary.left);
    Operand right = resolve_expr_rvalue(expr->binary.right);
    TokenKind op = expr->binary.op;
    const char *op_name = token_kind_name(op);
    return resolve_expr_binary_op(op, op_name, expr->pos, left, right);
}

Operand resolve_expr_compound(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_COMPOUND);
    if (!expected_type && !expr->compound.type) {
        fatal_error(expr->pos, "Implicitly typed compound literals used in context without expected type");
    }
    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
    } else {
        type = expected_type;
    }
    complete_type(type);
    bool is_const = is_const_type(type);
    type = unqualify_type(type);
    if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
        int index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == FIELD_INDEX) {
                fatal_error(field.pos, "Index field initializer not allowed for struct/union compound literal");
            } else if (field.kind == FIELD_NAME) {
                index = aggregate_field_index(type, field.name);
                if (index == -1) {
                    fatal_error(field.pos, "Named field in compound literal does not exist");
                }
            }
            if (index >= (int)type->aggregate.num_fields) {
                fatal_error(field.pos, "Field initializer in struct/union compound literal out of range");
            }
            Type *field_type = type->aggregate.fields[index].type;
            if (!resolve_typed_init(field.pos, field_type, field.init)) {
                fatal_error(field.pos, "Invalid type in compound literal initializer for aggregate type. Expected %s", get_type_name(field_type));
            }
            index++;
        }
    } else if (type->kind == TYPE_ARRAY) {
        int index = 0, max_index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == FIELD_NAME) {
                fatal_error(field.pos, "Named field initializer not allowed for array compound literals");
            } else if (field.kind == FIELD_INDEX) {
                Operand operand = resolve_const_expr(field.index);
                if (!is_integer_type(operand.type)) {
                    fatal_error(field.pos, "Field initializer index expression must have type int");
                }
                if (!cast_operand(&operand, type_int)) {
                    fatal_error(field.pos, "Invalid type in field initializer index. Expected integer type");
                }
                if (operand.val.i < 0) {
                    fatal_error(field.pos, "Field initializer index cannot be negative");
                }
                index = operand.val.i;
            }
            if (type->num_elems && index >= (int)type->num_elems) {
                fatal_error(field.pos, "Field initializer in array compound literal out of range");
            }
            if (!resolve_typed_init(field.pos, type->base, field.init)) {
                fatal_error(field.pos, "Invalid type in compound literal initializer for array type. Expected %s", get_type_name(type->base));
            }
            max_index = MAX(max_index, index);
            index++;
        }
        if (type->num_elems == 0) {
            type = type_array(type->base, max_index + 1);
        }
    } else {
        assert(is_scalar_type(type));
        if (expr->compound.num_fields > 1) {
            fatal_error(expr->pos, "Compound literal for scalar type cannot have more than one operand");
        }
        if (expr->compound.num_fields == 1) {
            CompoundField field = expr->compound.fields[0];
            Operand init = resolve_expected_expr_rvalue(field.init, type);
            if (!convert_operand(&init, type)) {
                fatal_error(field.pos, "Invalid type in compound literal initializer. Expected %s, got %s", get_type_name(type), get_type_name(init.type));
            }
        }
    }
    return operand_lvalue(is_const ? type_const(type) : type);
}

Operand resolve_expr_call(Expr *expr) {
    assert(expr->kind == EXPR_CALL);
    if (expr->call.expr->kind == EXPR_NAME) {
        Sym *sym = resolve_name(expr->call.expr->name);
        if (sym && sym->kind == SYM_TYPE) {
            if (expr->call.num_args != 1) {
                fatal_error(expr->pos, "Type conversion operator takes 1 argument");
            }
            Operand operand = resolve_expr_rvalue(expr->call.args[0]);
            if (!cast_operand(&operand, sym->type)) {
                fatal_error(expr->pos, "Invalid type cast from %s to %s", get_type_name(operand.type), get_type_name(sym->type));
            }
            set_resolved_sym(expr->call.expr, sym);
            return operand;
        }
    }
    Operand func = resolve_expr_rvalue(expr->call.expr);
    if (func.type->kind != TYPE_FUNC) {
        fatal_error(expr->pos, "Cannot call non-function value");
    }
    size_t num_params = func.type->func.num_params;
    if (expr->call.num_args < num_params) {
        fatal_error(expr->pos, "Function call with too few arguments");
    }
    if (expr->call.num_args > num_params && !func.type->func.has_varargs) {
        fatal_error(expr->pos, "Function call with too many arguments");
    }
    for (size_t i = 0; i < num_params; i++) {
        Type *param_type = func.type->func.params[i];
        Operand arg = resolve_expected_expr_rvalue(expr->call.args[i], param_type);
        if (is_array_type(param_type)) {
            param_type = type_ptr(param_type->base);
        }
        if (!convert_operand(&arg, param_type)) {
            fatal_error(expr->call.args[i]->pos, "Invalid type in function call argument. Expected %s, got %s", get_type_name(param_type), get_type_name(arg.type));
        }
    }
    for (size_t i = num_params; i < expr->call.num_args; i++) {
        resolve_expr_rvalue(expr->call.args[i]);
    }
    return operand_rvalue(func.type->func.ret);
}

Operand resolve_expr_ternary(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_TERNARY);
    Operand cond = resolve_expr_rvalue(expr->ternary.cond);
    if (!is_scalar_type(cond.type)) {
        fatal_error(expr->pos, "Ternary conditional must have scalar type");
    }
    Operand left = resolve_expected_expr_rvalue(expr->ternary.then_expr, expected_type);
    Operand right = resolve_expected_expr_rvalue(expr->ternary.else_expr, expected_type);
    if (is_arithmetic_type(left.type) && is_arithmetic_type(right.type)) {
        unify_arithmetic_operands(&left, &right);
        if (cond.is_const && left.is_const && right.is_const) {
            return operand_const(left.type, cond.val.i ? left.val : right.val);
        } else {
            return operand_rvalue(left.type);
        }
    } else if (left.type == right.type) {
        return operand_rvalue(left.type);
    } else {
        fatal_error(expr->pos, "Left and right operands of ternary expression must have arithmetic types or identical types");
    }
}

Operand resolve_expr_index(Expr *expr) {
    assert(expr->kind == EXPR_INDEX);
    Operand operand = resolve_expr_rvalue(expr->index.expr);
    if (!is_ptr_type(operand.type)) {
        fatal_error(expr->pos, "Can only index arrays and pointers");
    }
    Operand index = resolve_expr_rvalue(expr->index.index);
    if (!is_integer_type(index.type)) {
        fatal_error(expr->pos, "Index must have integer type");
    }
    return operand_lvalue(operand.type->base);
}

Operand resolve_expr_cast(Expr *expr) {
    assert(expr->kind == EXPR_CAST);
    Type *type = resolve_typespec(expr->cast.type);
    Operand operand = resolve_expr_rvalue(expr->cast.expr);
    if (!cast_operand(&operand, type)) {
        fatal_error(expr->pos, "Invalid type cast from %s to %s", get_type_name(operand.type), get_type_name(type));
    }
    return operand;
}

Operand resolve_expr_int(Expr *expr) {
    assert(expr->kind == EXPR_INT);
    unsigned long long int_max = type_metrics[TYPE_INT].max;
    unsigned long long uint_max = type_metrics[TYPE_UINT].max;
    unsigned long long long_max = type_metrics[TYPE_LONG].max;
    unsigned long long ulong_max = type_metrics[TYPE_ULONG].max;
    unsigned long long llong_max = type_metrics[TYPE_LLONG].max;
    unsigned long long val = expr->int_lit.val;
    Operand operand = operand_const(type_ullong, (Val){.ull = val});
    Type *type = type_ullong;
    if (expr->int_lit.mod == MOD_NONE) {
        bool overflow = false;
        switch (expr->int_lit.suffix) {
        case SUFFIX_NONE:
            type = type_int;
            if (val > int_max) {
                type = type_long;
                if (val > long_max) {
                    type = type_llong;
                    overflow = val > llong_max;
                }
            }
            break;
        case SUFFIX_U:
            type = type_uint;
            if (val > uint_max) {
                type = type_ulong;
                if (val > ulong_max) {
                    type = type_ullong;
                }
            }
            break;
        case SUFFIX_L:
            type = type_long;
            if (val > long_max) {
                type = type_llong;
                overflow = val > llong_max;
            }
            break;
        case SUFFIX_UL:
            type = type_ulong;
            if (val > ulong_max) {
                type = type_ullong;
            }
            break;
        case SUFFIX_LL:
            type = type_llong;
            overflow = val > llong_max;
            break;
        case SUFFIX_ULL:
            type = type_ullong;
            break;
        default:
            assert(0);
            break;
        }
        if (overflow) {
            fatal_error(expr->pos, "Integer literal overflow");
        }
    } else {
        switch (expr->int_lit.suffix) {
        case SUFFIX_NONE:
            type = type_int;
            if (val > int_max) {
                type = type_uint;
                if (val > uint_max) {
                    type = type_long;
                    if (val > long_max) {
                        type = type_ulong;
                        if (val > ulong_max) {
                            type = type_llong;
                            if (val > llong_max) {
                                type = type_ullong;
                            }
                        }
                    }
                }
            }
            break;
        case SUFFIX_U:
            type = type_uint;
            if (val > uint_max) {
                type = type_ulong;
                if (val > ulong_max) {
                    type = type_ullong;
                }
            }
            break;
        case SUFFIX_L:
            type = type_long;
            if (val > long_max) {
                type = type_ulong;
                if (val > ulong_max) {
                    type = type_llong;
                    if (val > llong_max) {
                        type = type_ullong;
                    }
                }
            }
            break;
        case SUFFIX_UL:
            type = type_ulong;
            if (val > ulong_max) {
                type = type_ullong;
            }
            break;
        case SUFFIX_LL:
            type = type_llong;
            if (val > llong_max) {
                type = type_ullong;
            }
            break;
        case SUFFIX_ULL:
            type = type_ullong;
            break;
        default:
            assert(0);
            break;
        }
    }
    cast_operand(&operand, type);
    return operand;
}

Operand resolve_expr_modify(Expr *expr) {
    Operand operand = resolve_expr(expr->modify.expr);
    Type *type = operand.type;
    complete_type(type);
    if (!operand.is_lvalue) {
        fatal_error(expr->pos, "Cannot modify non-lvalue");
    }
    if (type->nonmodifiable) {
        fatal_error(expr->pos, "Cannot modify non-modifiable type");
    }
    if (!(is_integer_type(type) || type->kind == TYPE_PTR)) {
        fatal_error(expr->pos, "%s only valid for integer and pointer types", token_kind_name(expr->modify.op));
    }
    return operand_rvalue(type);
}

Operand resolve_expected_expr(Expr *expr, Type *expected_type) {
    Operand result;
    switch (expr->kind) {
    case EXPR_PAREN:
        result = resolve_expected_expr(expr->paren.expr, expected_type);
        break;
    case EXPR_INT:
        result = resolve_expr_int(expr);
        break;  
    case EXPR_FLOAT:
        result = operand_const(expr->float_lit.suffix == SUFFIX_D ? type_double : type_float, (Val){0});
        break;
    case EXPR_STR:
        result = operand_rvalue(type_ptr(type_char));
        break;
    case EXPR_NAME:
        // HACK
        result = resolve_expr_name(expr);
        set_resolved_sym(expr, resolve_name(expr->name));
        break;
    case EXPR_CAST:
        result = resolve_expr_cast(expr);
        break;
    case EXPR_CALL:
        result = resolve_expr_call(expr);
        break;
    case EXPR_INDEX:
        result = resolve_expr_index(expr);
        break;
    case EXPR_FIELD:
        result = resolve_expr_field(expr);
        break;
    case EXPR_COMPOUND:
        result = resolve_expr_compound(expr, expected_type);
        break;
    case EXPR_UNARY:
        if (expr->unary.op == TOKEN_AND) {
            Operand operand;
            if (expected_type && is_ptr_type(expected_type)) {
                operand = resolve_expected_expr(expr->unary.expr, expected_type->base);
            } else {
                operand = resolve_expr(expr->unary.expr);
            }
            if (!operand.is_lvalue) {
                fatal_error(expr->pos, "Cannot take address of non-lvalue");
            }
            result = operand_rvalue(type_ptr(operand.type));
        } else {
            result = resolve_expr_unary(expr);
        }
        break;
    case EXPR_BINARY:
        result = resolve_expr_binary(expr);
        break;
    case EXPR_TERNARY:
        result = resolve_expr_ternary(expr, expected_type);
        break;
    case EXPR_SIZEOF_EXPR: {
        if (expr->sizeof_expr->kind == EXPR_NAME) {
            Sym *sym = resolve_name(expr->sizeof_expr->name);
            if (sym && sym->kind == SYM_TYPE) {
                complete_type(sym->type);
                result = operand_const(type_usize, (Val){.ull = type_sizeof(sym->type)});
                set_resolved_type(expr->sizeof_expr, sym->type);
                set_resolved_sym(expr->sizeof_expr, sym);
                break;
            }
        }
        Type *type = resolve_expr(expr->sizeof_expr).type;
        complete_type(type);
        result = operand_const(type_usize, (Val){.ull = type_sizeof(type)});
        break;
    }
    case EXPR_SIZEOF_TYPE: {
        Type *type = resolve_typespec(expr->sizeof_type);
        complete_type(type);
        result = operand_const(type_usize, (Val){.ull = type_sizeof(type)});
        break;
    }
    case EXPR_ALIGNOF_EXPR: {
        if (expr->sizeof_expr->kind == EXPR_NAME) {
            Sym *sym = resolve_name(expr->alignof_expr->name);
            if (sym && sym->kind == SYM_TYPE) {
                complete_type(sym->type);
                result = operand_const(type_usize, (Val){.ull = type_alignof(sym->type)});
                set_resolved_type(expr->alignof_expr, sym->type);
                set_resolved_sym(expr->alignof_expr, sym);
                break;
            }
        }
        Type *type = resolve_expr(expr->alignof_expr).type;
        complete_type(type);
        result = operand_const(type_usize, (Val){.ull = type_alignof(type)});
        break;
    }
    case EXPR_ALIGNOF_TYPE: {
        Type *type = resolve_typespec(expr->alignof_type);
        complete_type(type);
        result = operand_const(type_usize, (Val){.ull = type_alignof(type)});
        break;
    }
    case EXPR_TYPEOF_TYPE: {
        Type *type = resolve_typespec(expr->typeof_type);
        result = operand_const(type_ullong, (Val){.ull = type->typeid});
        break;
    }
    case EXPR_TYPEOF_EXPR: {
        if (expr->typeof_expr->kind == EXPR_NAME) {
            Sym *sym = resolve_name(expr->typeof_expr->name);
            if (sym && sym->kind == SYM_TYPE) {
                result = operand_const(type_ullong, (Val){.ull = sym->type->typeid});
                set_resolved_type(expr->typeof_expr, sym->type);
                set_resolved_sym(expr->typeof_expr, sym);
                break;
            }
        }
        Type *type = resolve_expr(expr->typeof_expr).type;
        result = operand_const(type_ullong, (Val){.ull = type->typeid});
        break;
    }
    case EXPR_OFFSETOF: {
        Type *type = resolve_typespec(expr->offsetof_field.type);
        complete_type(type);
        if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
            fatal_error(expr->pos, "offsetof can only be used with struct/union types");
        }
        int field = aggregate_field_index(type, expr->offsetof_field.name);
        if (field < 0) {
            fatal_error(expr->pos, "No field '%s' in type", expr->offsetof_field.name);
        }
        result = operand_const(type_usize, (Val){.ull = type->aggregate.fields[field].offset});
        break;
    }
    case EXPR_MODIFY:
        result = resolve_expr_modify(expr);
        break;
    default:
        assert(0);
        result = operand_null;
        break;
    }
    set_resolved_type(expr, result.type);
    return result;
}

Operand resolve_const_expr(Expr *expr) {
    Operand operand = resolve_expr(expr);
    if (!operand.is_const) {
        fatal_error(expr->pos, "Expected constant expression");
    }
    return operand;
}

Map decl_note_names;

void init_builtin_syms() {
    assert(current_package);
    sym_global_type("void", type_void);
    sym_global_type("bool", type_bool);
    sym_global_type("char", type_char);
    sym_global_type("schar", type_schar);
    sym_global_type("uchar", type_uchar);
    sym_global_type("short", type_short);
    sym_global_type("ushort", type_ushort);
    sym_global_type("int", type_int);
    sym_global_type("uint", type_uint);
    sym_global_type("long", type_long);
    sym_global_type("ulong", type_ulong);
    sym_global_type("llong", type_llong);
    sym_global_type("ullong", type_ullong);
    sym_global_type("float", type_float);
    sym_global_type("double", type_double);
}

void add_package_decls(Package *package) {
    for (size_t i = 0; i < package->num_decls; i++) {
        Decl *decl = package->decls[i];
        if (decl->kind == DECL_NOTE) {
            if (!map_get(&decl_note_names, decl->note.name)) {
                warning(decl->pos, "Unknown declaration #directive '%s'", decl->note.name);
            }
            if (decl->note.name == declare_note_name) {
                if (decl->note.num_args != 1) {
                    fatal_error(decl->pos, "#declare_note takes 1 argument");
                }
                Expr *arg = decl->note.args[0].expr;
                if (arg->kind != EXPR_NAME) {
                    fatal_error(decl->pos, "#declare_note argument must be name");
                }
                map_put(&decl_note_names, arg->name, (void *)1);
            } else if (decl->note.name == static_assert_name) {
                // TODO: decide how to handle top-level static asserts wrt laziness/tree shaking
                if (!flag_lazy) {
                    resolve_static_assert(decl->note);
                }
            }
        } else if (decl->kind == DECL_IMPORT) {
            // Add to list of imports
        } else {
            sym_global_decl(decl);
        }
    }
}

bool compile_package(Package *package);

extern const char **package_search_paths;
extern int num_package_search_paths;

bool is_package_dir(const char *search_path, const char *package_path) {
    char path[MAX_PATH];
    path_copy(path, search_path);
    path_join(path, package_path);
    DirListIter iter;
    for (dir_list(&iter, path); iter.valid; dir_list_next(&iter)) {
        const char *ext = path_ext(iter.name);
        if (ext != iter.name && strcmp(ext, "ion") == 0) {
            dir_list_free(&iter);
            return true;
        }
    }
    return false;
}

bool copy_package_full_path(char dest[MAX_PATH], const char *package_path) {
    for (int i = 0; i < num_package_search_paths; i++) {
        if (is_package_dir(package_search_paths[i], package_path)) {
            path_copy(dest, package_search_paths[i]);
            path_join(dest, package_path);
            return true;
        }
    }
    return false;
}

Package *import_package(const char *package_path) {
    package_path = str_intern(package_path);
    Package *package = map_get(&package_map, package_path);
    if (!package) {
        package = xcalloc(1, sizeof(Package));
        package->path = package_path;
        if (flag_verbose) {
            printf("Importing %s\n", package_path);
        }
        char full_path[MAX_PATH];
        if (!copy_package_full_path(full_path, package_path)) {
            return NULL;
        }
        strcpy(package->full_path, full_path);
        add_package(package);
        compile_package(package);
    }
    return package;
}

void import_all_package_symbols(Package *package) {
    // TODO: should have a more general mechanism
    const char *main_name = str_intern("main");
    for (size_t i = 0; i < buf_len(package->syms); i++) {
        if (package->syms[i]->package == package && package->syms[i]->name != main_name) {
            sym_global_put(package->syms[i]->name, package->syms[i]);
        }
    }
}

void import_package_symbols(Decl *decl, Package *package) {
    for (size_t i = 0; i < decl->import.num_items; i++) {
        ImportItem item = decl->import.items[i];
        Sym *sym = get_package_sym(package, item.name);
        if (!sym) {
            fatal_error(decl->pos, "Symbol '%s' does not exist in package '%s'", item.name, package->path);
        }
        sym_global_put(item.rename ? item.rename : item.name, sym);
    }
}

void process_package_imports(Package *package) {
    for (size_t i = 0; i < package->num_decls; i++) {
        Decl *decl = package->decls[i];
        if (decl->kind == DECL_NOTE) {
            if (decl->note.name == always_name) {
                package->always_reachable = true;
            }
        } else if (decl->kind == DECL_IMPORT) {
            char *path_buf = NULL;
            if (decl->import.is_relative) {
                buf_printf(path_buf, "%s/", package->path);
            }
            for (size_t k = 0; k < decl->import.num_names; k++) {
                buf_printf(path_buf, "%s%s", k == 0 ? "" : "/", decl->import.names[k]);
            }
            Package *imported_package = import_package(path_buf);
            if (!imported_package) {
                fatal_error(decl->pos, "Failed to import package '%s'", path_buf);
            }
            buf_free(path_buf);
            import_package_symbols(decl, imported_package);
            if (decl->import.import_all) {
                import_all_package_symbols(imported_package);
            }
        }
    }
}

bool parse_package(Package *package) {
    Decl **decls = NULL;
    DirListIter iter;
    for (dir_list(&iter, package->full_path); iter.valid; dir_list_next(&iter)) {
        if (iter.is_dir || iter.name[0] == '_' || iter.name[0] == '.') {
            continue;
        }
        char name[MAX_PATH];
        path_copy(name, iter.name);
        char *ext = path_ext(name);
        if (ext == name || strcmp(ext, "ion") != 0) {
            continue;
        }
        ext[-1] = 0;
        if (is_excluded_target_filename(name)) {
            continue;
        }
        char path[MAX_PATH];
        path_copy(path, iter.base);
        path_join(path, iter.name);
        path_absolute(path);
        const char *code = read_file(path);
        if (!code) {
            fatal_error((SrcPos){.name = path}, "Failed to read source file");
        }
        init_stream(str_intern(path), code);
        Decls *file_decls = parse_decls();
        for (size_t i = 0; i < file_decls->num_decls; i++) {
            buf_push(decls, file_decls->decls[i]);
        }
    }
    package->decls = decls;
    package->num_decls = (int)buf_len(decls);
    return package;
}

bool compile_package(Package *package) {
    if (!parse_package(package)) {
        return false;
    }
    Package *old_package = enter_package(package);
    if (buf_len(package_list) == 1) {
        init_builtin_syms();
    }
    if (builtin_package) {
        import_all_package_symbols(builtin_package);
    }
    add_package_decls(package);
    process_package_imports(package);
    leave_package(old_package);
    return true;
}

void resolve_package_syms(Package *package) {
    Package *old_package = enter_package(package);
    for (size_t i = 0; i < buf_len(package->syms); i++) {
        if (package->syms[i]->package == package) {
            resolve_sym(package->syms[i]);
        }
    }
    leave_package(old_package);
}

void finalize_reachable_syms(void) {
    if (flag_verbose) {
        printf("Finalizing reachable symbols\n");
    }
    size_t prev_num_reachable = 0;
    size_t num_reachable = buf_len(reachable_syms);
    for (size_t i = 0; i < num_reachable; i++) {
        finalize_sym(reachable_syms[i]);
        if (i == num_reachable - 1) {
            if (flag_verbose) {
                printf("New reachable symbols:");
                for (size_t k = prev_num_reachable; k < num_reachable; k++) {
                    printf(" %s/%s", reachable_syms[k]->package->path, reachable_syms[k]->name);
                }
                printf("\n");
            }
            prev_num_reachable = num_reachable;
            num_reachable = buf_len(reachable_syms);
        }
    }
}
