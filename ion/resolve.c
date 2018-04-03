typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
} TypeKind;

typedef struct Type Type;
typedef struct Sym Sym;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

struct Type {
    TypeKind kind;
    size_t size;
    size_t align;
    Sym *sym;
    union {
        struct {
            Type *elem;
        } ptr;
        struct {
            Type *elem;
            size_t size;
        } array;
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            Type **params;
            size_t num_params;
            Type *ret;
        } func;
    };
};

void complete_type(Type *type);

Type *type_alloc(TypeKind kind) {
    Type *type = xcalloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}

Type *type_void = &(Type){TYPE_VOID, 0};
Type *type_char = &(Type){TYPE_CHAR, 1, 1};
Type *type_int = &(Type){TYPE_INT, 4, 4};
Type *type_float = &(Type){TYPE_FLOAT, 4, 4};

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGN = 8;

size_t type_sizeof(Type *type) {
    assert(type->kind > TYPE_COMPLETING);
    assert(type->size != 0);
    return type->size;
}

size_t type_alignof(Type *type) {
    assert(type->kind > TYPE_COMPLETING);
    return type->align;
}

typedef struct CachedPtrType {
    Type *elem;
    Type *ptr;
} CachedPtrType;

CachedPtrType *cached_ptr_types;

Type *type_ptr(Type *elem) {
    for (CachedPtrType *it = cached_ptr_types; it != buf_end(cached_ptr_types); it++) {
        if (it->elem == elem) {
            return it->ptr;
        }
    }
    Type *type = type_alloc(TYPE_PTR);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->ptr.elem = elem;
    buf_push(cached_ptr_types, (CachedPtrType){elem, type});
    return type;
}

typedef struct CachedArrayType {
    Type *elem;
    size_t size;
    Type *array;
} CachedArrayType;

CachedArrayType *cached_array_types;

Type *type_array(Type *elem, size_t size) {
    for (CachedArrayType *it = cached_array_types; it != buf_end(cached_array_types); it++) {
        if (it->elem == elem && it->size == size) {
            return it->array;
        }
    }
    complete_type(elem);
    Type *type = type_alloc(TYPE_ARRAY);
    type->size = size * type_sizeof(elem);
    type->align = type_alignof(elem);
    type->array.elem = elem;
    type->array.size = size;
    buf_push(cached_array_types, (CachedArrayType){elem, size, type});
    return type;
}

typedef struct CachedFuncType {
    Type **params;
    size_t num_params;
    Type *ret;
    Type *func;
} CachedFuncType;

CachedFuncType *cached_func_types;

Type *type_func(Type **params, size_t num_params, Type *ret) {
    for (CachedFuncType *it = cached_func_types; it != buf_end(cached_func_types); it++) {
        if (it->num_params == num_params && it->ret == ret) {
            bool match = true;
            for (size_t i = 0; i < num_params; i++) {
                if (it->params[i] != params[i]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                return it->func;
            }
        }
    }
    Type *type = type_alloc(TYPE_FUNC);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->func.params = memdup(params, num_params * sizeof(*params));
    type->func.num_params = num_params;
    type->func.ret = ret;
    buf_push(cached_func_types, (CachedFuncType){params, num_params, ret, type});
    return type;
}

// TODO: This probably shouldn't use an O(n^2) algorithm
bool duplicate_fields(TypeField *fields, size_t num_fields) {
    for (size_t i = 0; i < num_fields; i++) {
        for (size_t j = i+1; j < num_fields; j++) {
            if (fields[i].name == fields[j].name) {
                return true;
            }
        }
    }
    return false;
}

void type_complete_struct(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(IS_POW2(type_alignof(it->type)));
        type->size = type_sizeof(it->type) + ALIGN_UP(type->size, type_alignof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
    }
    type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
    type->aggregate.num_fields = num_fields;
}

void type_complete_union(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_UNION;
    type->size = 0;
    type->align = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(it->type->kind > TYPE_COMPLETING);
        type->size = MAX(type->size, type_sizeof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
    }
    type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
    type->aggregate.num_fields = num_fields;
}

Type *type_incomplete(Sym *sym) {
    Type *type = type_alloc(TYPE_INCOMPLETE);
    type->sym = sym;
    return type;
}

typedef enum SymKind {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FUNC,
    SYM_TYPE,
    SYM_ENUM_CONST,
} SymKind;

typedef enum SymState {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

typedef struct Sym {
    const char *name;
    SymKind kind;
    SymState state;
    Decl *decl;
    Type *type;
    int64_t val;
} Sym;

enum {
    MAX_LOCAL_SYMS = 1024
};

Sym **global_syms;
Sym local_syms[MAX_LOCAL_SYMS];
Sym *local_syms_end = local_syms;

Sym *sym_new(SymKind kind, const char *name, Decl *decl) {
    Sym *sym = xcalloc(1, sizeof(Sym));
    sym->kind = kind;
    sym->name = name;
    sym->decl = decl;
    return sym;
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
    if (decl->kind == DECL_STRUCT || decl->kind == DECL_UNION) {
        sym->state = SYM_RESOLVED;
        sym->type = type_incomplete(sym);
    }
    return sym;
}

Sym *sym_enum_const(const char *name, Decl *decl) {
    return sym_new(SYM_ENUM_CONST, name, decl);
}

Sym *sym_get(const char *name) {
    for (Sym *it = local_syms_end; it != local_syms; it--) {
        Sym *sym = it-1;
        if (sym->name == name) {
            return sym;
        }
    }
    for (Sym **it = global_syms; it != buf_end(global_syms); it++) {
        Sym *sym = *it;
        if (sym->name == name) {
            return sym;
        }
    }
    return NULL;
}

void sym_push_var(const char *name, Type *type) {
    if (local_syms_end == local_syms + MAX_LOCAL_SYMS) {
        fatal("Too many local symbols");
    }
    *local_syms_end++ = (Sym){
        .name = name,
        .kind = SYM_VAR,
        .state = SYM_RESOLVED,
        .type = type,
    };
}

Sym *sym_enter(void) {
    return local_syms_end;
}

void sym_leave(Sym *sym) {
    local_syms_end = sym;
}

Sym *sym_global_decl(Decl *decl) {
    Sym *sym = sym_decl(decl);
    buf_push(global_syms, sym);
    decl->sym = sym;
    if (decl->kind == DECL_ENUM) {
        for (size_t i = 0; i < decl->enum_decl.num_items; i++) {
            buf_push(global_syms, sym_enum_const(decl->enum_decl.items[i].name, decl));
        }
    }
    return sym;
}

Sym *sym_global_type(const char *name, Type *type) {
    Sym *sym = sym_new(SYM_TYPE, name, NULL);
    sym->state = SYM_RESOLVED;
    sym->type = type;
    buf_push(global_syms, sym);
    return sym;
}

typedef struct Operand {
    Type *type;
    bool is_lvalue;
    bool is_const;
    int64_t val;
} Operand;

Operand operand_null;

Operand operand_rvalue(Type *type) {
    return (Operand){
        .type = type,
    };
}

Operand operand_lvalue(Type *type) {
    return (Operand){
        .type = type,
        .is_lvalue = true,
    };
}

Operand operand_const(int64_t val) {
    return (Operand){
        .type = type_int,
        .is_const = true,
        .val = val,
    };
}

Sym *resolve_name(const char *name);
int64_t resolve_const_expr(Expr *expr);
Operand resolve_expr(Expr *expr);
Operand resolve_expected_expr(Expr *expr, Type *expected_type);

Type *resolve_typespec(Typespec *typespec) {
    if (!typespec) {
        return type_void;
    }
    Type *result = NULL;
    switch (typespec->kind) {
    case TYPESPEC_NAME: {
        Sym *sym = resolve_name(typespec->name);
        if (sym->kind != SYM_TYPE) {
            fatal("%s must denote a type", typespec->name);
            return NULL;
        }
        result = sym->type;
        break;
    }
    case TYPESPEC_PTR:
        result = type_ptr(resolve_typespec(typespec->ptr.elem));
        break;
    case TYPESPEC_ARRAY: {
        int64_t size = resolve_const_expr(typespec->array.size);
        if (size < 0) {
            fatal("Negative array size");
        }
        result = type_array(resolve_typespec(typespec->array.elem), size);
        break;
    }
    case TYPESPEC_FUNC: {
        Type **args = NULL;
        for (size_t i = 0; i < typespec->func.num_args; i++) {
            buf_push(args, resolve_typespec(typespec->func.args[i]));
        }
        Type *ret = type_void;
        if (typespec->func.ret) {
            ret = resolve_typespec(typespec->func.ret);
        }
        result = type_func(args, buf_len(args), ret);
        break;
    }
    default:
        assert(0);
        return NULL;
    }
    assert(!typespec->type || typespec->type == result);
    typespec->type = result;
    return result;
}

Sym **ordered_syms;

void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal("Type completion cycle");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }
    type->kind = TYPE_COMPLETING;
    Decl *decl = type->sym->decl;
    assert(decl->kind == DECL_STRUCT || decl->kind == DECL_UNION);
    TypeField *fields = NULL;
    for (size_t i = 0; i < decl->aggregate.num_items; i++) {
        AggregateItem item = decl->aggregate.items[i];
        Type *item_type = resolve_typespec(item.type);
        complete_type(item_type);
        for (size_t j = 0; j < item.num_names; j++) {
            buf_push(fields, (TypeField){item.names[j], item_type});
        }
    }
    if (buf_len(fields) == 0) {
        fatal("No fields");
    }
    if (duplicate_fields(fields, buf_len(fields))) {
        fatal("Duplicate fields");
    }
    if (decl->kind == DECL_STRUCT) {
        type_complete_struct(type, fields, buf_len(fields));
    } else {
        assert(decl->kind == DECL_UNION);
        type_complete_union(type, fields, buf_len(fields));
    }
    buf_push(ordered_syms, type->sym);
}

Type *resolve_decl_type(Decl *decl) {
    assert(decl->kind == DECL_TYPEDEF);
    return resolve_typespec(decl->typedef_decl.type);
}

Type *resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);
    Type *type = NULL;
    if (decl->var.type) {
        type = resolve_typespec(decl->var.type);
    }
    if (decl->var.expr) {
        Operand result = resolve_expected_expr(decl->var.expr, type);
        if (type && result.type != type) {
            fatal("Declared var type does not match inferred type");
        }
        type = result.type;
    }
    complete_type(type);
    return type;
}

Type *resolve_decl_const(Decl *decl, int64_t *val) {
    assert(decl->kind == DECL_CONST);
    Operand result = resolve_expr(decl->const_decl.expr);
    if (!result.is_const) {
        fatal("Initializer for const is not a constant expression");
    }
    *val = result.val;
    return result.type;
}

Type *resolve_decl_func(Decl *decl) {
    assert(decl->kind == DECL_FUNC);
    Type **params = NULL;
    for (size_t i = 0; i < decl->func.num_params; i++) {
        buf_push(params, resolve_typespec(decl->func.params[i].type));
    }
    Type *ret_type = type_void;
    if (decl->func.ret_type) {
        ret_type = resolve_typespec(decl->func.ret_type);
    }
    return type_func(params, buf_len(params), ret_type);
}

void resolve_stmt(Stmt *stmt, Type *ret_type);

void resolve_cond_expr(Expr *expr) {
    Operand cond = resolve_expr(expr);
    if (cond.type != type_int) {
        fatal("Conditional expression must have type int");
    }
}

void resolve_stmt_block(StmtList block, Type *ret_type) {
    Sym *scope = sym_enter();
    for (size_t i = 0; i < block.num_stmts; i++) {
        resolve_stmt(block.stmts[i], ret_type);
    }
    sym_leave(scope);
}

void resolve_stmt(Stmt *stmt, Type *ret_type) {
    switch (stmt->kind) {
    case STMT_RETURN:
        if (stmt->expr) {
            if (resolve_expected_expr(stmt->expr, ret_type).type != ret_type) {
                fatal("Return type mismatch");
            }
        } else if (ret_type != type_void) {
            fatal("Empty return expression for function with non-void return type");
        }
        break;
    case STMT_BREAK:
    case STMT_CONTINUE:
        // Do nothing
        break;
    case STMT_BLOCK:
        resolve_stmt_block(stmt->block, ret_type);
        break;
    case STMT_IF:
        resolve_cond_expr(stmt->if_stmt.cond);
        resolve_stmt_block(stmt->if_stmt.then_block, ret_type);
        for (size_t i = 0; i < stmt->if_stmt.num_elseifs; i++) {
            ElseIf elseif = stmt->if_stmt.elseifs[i];
            resolve_cond_expr(elseif.cond);
            resolve_stmt_block(elseif.block, ret_type);
        }
        if (stmt->if_stmt.else_block.stmts) {
            resolve_stmt_block(stmt->if_stmt.else_block, ret_type);
        }
        break;
    case STMT_WHILE:
    case STMT_DO_WHILE:
        resolve_cond_expr(stmt->while_stmt.cond);
        resolve_stmt_block(stmt->while_stmt.block, ret_type);
        break;
    case STMT_FOR: {
        Sym *scope = sym_enter();
        resolve_stmt(stmt->for_stmt.init, ret_type);
        resolve_cond_expr(stmt->for_stmt.cond);
        resolve_stmt_block(stmt->for_stmt.block, ret_type);
        resolve_stmt(stmt->for_stmt.next, ret_type);
        sym_leave(scope);
        break;
    }
    case STMT_SWITCH: {
        Operand expr = resolve_expr(stmt->switch_stmt.expr);
        for (size_t i = 0; i < stmt->switch_stmt.num_cases; i++) {
            SwitchCase switch_case = stmt->switch_stmt.cases[i];
            for (size_t j = 0; j < switch_case.num_exprs; j++) {
                if (resolve_expr(switch_case.exprs[j]).type != expr.type) {
                    fatal("Switch case expression type mismatch");
                }
                resolve_stmt_block(switch_case.block, ret_type);
            }
        }
        break;
    }
    case STMT_ASSIGN: {
        Operand left = resolve_expr(stmt->assign.left);
        if (stmt->assign.right && resolve_expected_expr(stmt->assign.right, left.type).type != left.type) {
            fatal("Left/right types do not match in assignment statement");
        }
        if (!left.is_lvalue) {
            fatal("Cannot assign to non-lvalue");
        }
        if (stmt->assign.op != TOKEN_ASSIGN && left.type != type_int) {
            fatal("Can only use %s with type int", token_kind_name(stmt->assign.op));
        }
        break;
    }
    case STMT_INIT:
        sym_push_var(stmt->init.name, resolve_expr(stmt->init.expr).type);
        break;
    default:
        assert(0);
        break;
    }
}

void resolve_func_body(Sym *sym) {
    Decl *decl = sym->decl;
    assert(decl->kind == DECL_FUNC);
    assert(sym->state == SYM_RESOLVED);
    Sym *scope = sym_enter();
    for (size_t i = 0; i < decl->func.num_params; i++) {
        FuncParam param = decl->func.params[i];
        sym_push_var(param.name, resolve_typespec(param.type));
    }
    resolve_stmt_block(decl->func.block, resolve_typespec(decl->func.ret_type));
    sym_leave(scope);
}
    
void resolve_sym(Sym *sym) {
    if (sym->state == SYM_RESOLVED) {
        return;
    } else if (sym->state == SYM_RESOLVING) {
        fatal("Cyclic dependency");
        return;
    }
    assert(sym->state == SYM_UNRESOLVED);
    sym->state = SYM_RESOLVING;
    switch (sym->kind) {
    case SYM_TYPE:
        sym->type = resolve_decl_type(sym->decl);
        break;
    case SYM_VAR:
        sym->type = resolve_decl_var(sym->decl);
        break;
    case SYM_CONST:
        sym->type = resolve_decl_const(sym->decl, &sym->val);
        break;
    case SYM_FUNC:
        sym->type = resolve_decl_func(sym->decl);
        break;
    default:
        assert(0);
        break;
    }
    sym->state = SYM_RESOLVED;
    buf_push(ordered_syms, sym);
}

void finalize_sym(Sym *sym) {
    resolve_sym(sym);
    if (sym->kind == SYM_TYPE) {
        complete_type(sym->type);
    } else if (sym->kind == SYM_FUNC) {
        resolve_func_body(sym);
    }
}

Sym *resolve_name(const char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        fatal("Undeclared name '%s'", name);
        return NULL;
    }
    resolve_sym(sym);
    return sym;
}

Operand resolve_expr_field(Expr *expr) {
    assert(expr->kind == EXPR_FIELD);
    Operand left = resolve_expr(expr->field.expr);
    Type *type = left.type;
    complete_type(type);
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
        fatal("Can only access fields on aggregate types");
        return operand_null;
    }
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        TypeField field = type->aggregate.fields[i];
        if (field.name == expr->field.name) {
            return left.is_lvalue ? operand_lvalue(field.type) : operand_rvalue(field.type);
        }
    }
    fatal("No field named '%s'", expr->field.name);
    return operand_null;
}

Operand ptr_decay(Operand expr) {
    if (expr.type->kind == TYPE_ARRAY) {
        return operand_rvalue(type_ptr(expr.type->array.elem));
    } else {
        return expr;
    }
}

int64_t eval_int_unary(TokenKind op, int64_t val) {
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
        return 0;
    }
}

int64_t eval_int_binary(TokenKind op, int64_t left, int64_t right) {
    switch (op) {
    case TOKEN_MUL:
        return left * right;
    case TOKEN_DIV:
        return right != 0 ? left / right : 0;
    case TOKEN_MOD:
        return right != 0 ? left % right : 0;
    case TOKEN_AND:
        return left & right;
        // TODO: Don't allow UB in shifts, etc
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
        // TODO: Probably handle logical AND/OR separately
    case TOKEN_AND_AND:
        return left && right;
    case TOKEN_OR_OR:
        return left || right;
    default:
        assert(0);
        return 0;
    }
}

Operand resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    Sym *sym = resolve_name(expr->name);
    if (sym->kind == SYM_VAR) {
        return operand_lvalue(sym->type);
    } else if (sym->kind == SYM_CONST) {
        return operand_const(sym->val);
    } else if (sym->kind == SYM_FUNC) {
        return operand_rvalue(sym->type);
    } else {
        fatal("%s must be a var or const", expr->name);
        return operand_null;
    }
}
 
Operand resolve_expr_unary(Expr *expr) {
    assert(expr->kind == EXPR_UNARY);
    Operand operand = resolve_expr(expr->unary.expr);
    Type *type = operand.type;
    switch (expr->unary.op) {
    case TOKEN_MUL:
        operand = ptr_decay(operand);
        type = operand.type;
        if (type->kind != TYPE_PTR) {
            fatal("Cannot deref non-ptr type");
        }
        return operand_lvalue(type->ptr.elem);
    case TOKEN_AND:
        if (!operand.is_lvalue) {
            fatal("Cannot take address of non-lvalue");
        }
        return operand_rvalue(type_ptr(type));
    default:
        if (type->kind != TYPE_INT) {
            fatal("Can only use unary %s with ints", token_kind_name(expr->unary.op));
        }
        if (operand.is_const) {
            return operand_const(eval_int_unary(expr->unary.op, operand.val));
        } else {
            return operand_rvalue(type);
        }
    }
}

Operand resolve_expr_binary(Expr *expr) {
    assert(expr->kind == EXPR_BINARY);
    Operand left = resolve_expr(expr->binary.left);
    Operand right = resolve_expr(expr->binary.right);
    if (left.type != type_int) {
        fatal("left operand of + must be int");
    }
    if (right.type != left.type)  {
        fatal("left and right operand of + must have same type");
    }
    if (left.is_const && right.is_const) {
        return operand_const(eval_int_binary(expr->binary.op, left.val, right.val));
    } else {
        return operand_rvalue(left.type);
    }
}

size_t aggregate_field_index(Type *type, const char *name) {
    assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        if (type->aggregate.fields[i].name == name) {
            return i;
        }
    }
    fatal("Field '%s' in compound literal not found in struct/union", name);
    return SIZE_MAX;
}

Operand resolve_expr_compound(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_COMPOUND);
    if (!expected_type && !expr->compound.type) {
        fatal("Implicitly typed compound literals used in context without expected type");
    }
    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
    } else {
        type = expected_type;
    }
    complete_type(type);
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_ARRAY) {
        fatal("Compound literals can only be used with struct and array types");
    }
    if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
        size_t index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == FIELD_INDEX) {
                fatal("Index field initializer not allowed for struct/union compound literal");
            } else if (field.kind == FIELD_NAME) {
                index = aggregate_field_index(type, field.name);
            }
            if (index >= type->aggregate.num_fields) {
                fatal("Field initializer in struct/union compound literal out of range");
            }
            Operand init = resolve_expected_expr(expr->compound.fields[i].init, type->aggregate.fields[index].type);
            if (init.type != type->aggregate.fields[index].type) {
                fatal("Compound literal field type mismatch");
            }
            index++;
        }
    } else {
        assert(type->kind == TYPE_ARRAY);
        size_t index = 0;
        for (size_t i = 0; i < expr->compound.num_fields; i++) {
            CompoundField field = expr->compound.fields[i];
            if (field.kind == FIELD_NAME) {
                fatal("Named field initializer not allowed for array compound literals");
            } else if (field.kind == FIELD_INDEX) {
                int64_t result = resolve_const_expr(field.index);
                if (result < 0) {
                    fatal("Field initializer index cannot be negative");
                }
                index = result;
            }
            if (index >= type->array.size) {
                fatal("Field initializer in array compound literal out of range");
            }
            Operand init = resolve_expected_expr(expr->compound.fields[i].init, type->array.elem);
            if (init.type != type->array.elem) {
                fatal("Compound literal element type mismatch");
            }
            index++;
        }
    }
    return operand_rvalue(type);
}

Operand resolve_expr_call(Expr *expr) {
    assert(expr->kind == EXPR_CALL);
    Operand func = resolve_expr(expr->call.expr);
    if (func.type->kind != TYPE_FUNC) {
        fatal("Trying to call non-function value");
    }
    if (expr->call.num_args != func.type->func.num_params) {
        fatal("Tried to call function with wrong number of arguments");
    }
    for (size_t i = 0; i < expr->call.num_args; i++) {
        Type *param_type = func.type->func.params[i];
        Operand arg = resolve_expected_expr(expr->call.args[i], param_type);
        if (arg.type != param_type) {
            fatal("Call argument expression type doesn't match expected param type");
        }
    }
    return operand_rvalue(func.type->func.ret);
}

Operand resolve_expr_ternary(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_TERNARY);
    Operand cond = ptr_decay(resolve_expr(expr->ternary.cond));
    if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR) {
        fatal("Ternary cond expression must have type int or ptr");
    }
    Operand then_expr = ptr_decay(resolve_expected_expr(expr->ternary.then_expr, expected_type));
    Operand else_expr = ptr_decay(resolve_expected_expr(expr->ternary.else_expr, expected_type));
    if (then_expr.type != else_expr.type) {
        fatal("Ternary then/else expressions must have matching types");
    }
    if (cond.is_const && then_expr.is_const && else_expr.is_const) {
        return operand_const(cond.val ? then_expr.val : else_expr.val);
    } else {
        return operand_rvalue(then_expr.type);
    }
}

Operand resolve_expr_index(Expr *expr) {
    assert(expr->kind == EXPR_INDEX);
    Operand operand = ptr_decay(resolve_expr(expr->index.expr));
    if (operand.type->kind != TYPE_PTR) {
        fatal("Can only index arrays or pointers");
    }
    Operand index = resolve_expr(expr->index.index);
    if (index.type->kind != TYPE_INT) {
        fatal("Index expression must have type int");
    }
    return operand_lvalue(operand.type->ptr.elem);
}

Operand resolve_expr_cast(Expr *expr) {
    assert(expr->kind == EXPR_CAST);
    Type *type = resolve_typespec(expr->cast.type);
    Operand result = ptr_decay(resolve_expr(expr->cast.expr));
    if (type->kind == TYPE_PTR) {
        if (result.type->kind != TYPE_PTR && result.type->kind != TYPE_INT) {
            fatal("Invalid cast to pointer type");
        }
    } else if (type->kind == TYPE_INT) {
        if (result.type->kind != TYPE_PTR && result.type->kind != TYPE_INT) {
            fatal("Invalid cast to int type");
        }
    } else {
        fatal("Invalid target cast type");
    }
    return operand_rvalue(type);
}

Operand resolve_expected_expr(Expr *expr, Type *expected_type) {
    Operand result;
    switch (expr->kind) {
    case EXPR_INT:
        result = operand_const(expr->int_val);
        break;
    case EXPR_FLOAT:
        result = operand_rvalue(type_float);
        break;
    case EXPR_STR:
        result = operand_rvalue(type_ptr(type_char));
        break;
    case EXPR_NAME:
        result = resolve_expr_name(expr);
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
        result = resolve_expr_unary(expr);
        break;
    case EXPR_BINARY:
        result = resolve_expr_binary(expr);
        break;
    case EXPR_TERNARY:
        result = resolve_expr_ternary(expr, expected_type);
        break;
    case EXPR_SIZEOF_EXPR: {
        Type *type = resolve_expr(expr->sizeof_expr).type;
        complete_type(type);
        result = operand_const(type_sizeof(type));
        break;
    }
    case EXPR_SIZEOF_TYPE: {
        Type *type = resolve_typespec(expr->sizeof_type);
        complete_type(type);
        result = operand_const(type_sizeof(type));
        break;
    }
    default:
        assert(0);
        result = operand_null;
        break;
    }
    if (result.type) {
        assert(!expr->type || expr->type == result.type);
        expr->type = result.type;
    }
    return result;
}

Operand resolve_expr(Expr *expr) {
    return resolve_expected_expr(expr, NULL);
}

int64_t resolve_const_expr(Expr *expr) {
    Operand result = resolve_expr(expr);
    if (!result.is_const) {
        fatal("Expected constant expression");
    }
    return result.val;
}

void init_global_syms(void) {
    sym_global_type(str_intern("void"), type_void);
    sym_global_type(str_intern("char"), type_char);
    sym_global_type(str_intern("int"), type_int);
    sym_global_type(str_intern("float"), type_float);
}

void sym_global_decls(DeclSet *declset) {
    for (size_t i = 0; i < declset->num_decls; i++) {
        sym_global_decl(declset->decls[i]);
    }
}

void finalize_syms(void) {
    for (Sym **it = global_syms; it != buf_end(global_syms); it++) {
        Sym *sym = *it;
        finalize_sym(sym);
    }
}

void resolve_test(void) {
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
    Type *int_int_func = type_func(&type_int, 1, type_int);
    assert(type_func(&type_int, 1, type_int) == int_int_func);
    Type *int_func = type_func(NULL, 0, type_int);
    assert(int_int_func != int_func);
    assert(int_func == type_func(NULL, 0, type_int));

    init_global_syms();

    const char *code[] = {
        "union IntOrPtr { i: int; p: int*; }",
        "var u1 = IntOrPtr{i = 42}",
        "var u2 = IntOrPtr{p = cast(int*, 42)}",
        "var i: int",
        "struct Vector { x, y: int; }",
        "func f1() { v := Vector{1, 2}; j := i; i++; j++; v.x = 2*j; }",
        "func f2(n: int): int { return 2*n; }",
        "func f3(x: int): int { if (x) { return -x; } else if (x % 2 == 0) { return 42; } else { return -1; } }",
        "func f4(n: int): int { for (i := 0; i < n; i++) { if (i % 3 == 0) { return n; } } return 0; }",
        "func f5(x: int): int { switch(x) { case 0: case 1: return 42; case 3: default: return -1; } }",
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
        "var i = cast(int, p) + 1",
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
        "var j = cast(int, p)",
        "var q = cast(int*, j)",
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
        init_stream(code[i]);
        Decl *decl = parse_decl();
        sym_global_decl(decl);
    }
    finalize_syms();
    for (Sym **it = ordered_syms; it != buf_end(ordered_syms); it++) {
        Sym *sym = *it;
        if (sym->decl) {
            print_decl(sym->decl);
        } else {
            printf("%s", sym->name);
        }
        printf("\n");
    }
}
