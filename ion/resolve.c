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
typedef struct Entity Entity;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

struct Type {
    TypeKind kind;
    size_t size;
    size_t align;
    Entity *entity;
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
    assert(IS_POW2(type->align));
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

Type *type_incomplete(Entity *entity) {
    Type *type = type_alloc(TYPE_INCOMPLETE);
    type->entity = entity;
    return type;
}

typedef enum EntityKind {
    ENTITY_NONE,
    ENTITY_VAR,
    ENTITY_CONST,
    ENTITY_FUNC,
    ENTITY_TYPE,
    ENTITY_ENUM_CONST,
} EntityKind;

typedef enum EntityState {
    ENTITY_UNRESOLVED,
    ENTITY_RESOLVING,
    ENTITY_RESOLVED,
} EntityState;

typedef struct Entity {
    const char *name;
    EntityKind kind;
    EntityState state;
    Decl *decl;
    Type *type;
    int64_t val;
} Entity;

Entity **entities;

Entity *entity_new(EntityKind kind, const char *name, Decl *decl) {
    Entity *entity = xcalloc(1, sizeof(Decl));
    entity->kind = kind;
    entity->name = name;
    entity->decl = decl;
    return entity;
}

Entity *entity_decl(Decl *decl) {
    EntityKind kind = ENTITY_NONE;
    switch (decl->kind) {
    case DECL_STRUCT:
    case DECL_UNION:
    case DECL_TYPEDEF:
    case DECL_ENUM:
        kind = ENTITY_TYPE;
        break;
    case DECL_VAR:
        kind = ENTITY_VAR;
        break;
    case DECL_CONST:
        kind = ENTITY_CONST;
        break;
    case DECL_FUNC:
        kind = ENTITY_FUNC;
        break;
    default:
        assert(0);
        break;
    }
    Entity *entity = entity_new(kind, decl->name, decl);
    if (decl->kind == DECL_STRUCT || decl->kind == DECL_UNION) {
        entity->state = ENTITY_RESOLVED;
        entity->type = type_incomplete(entity);
    }
    return entity;
}

Entity *entity_enum_const(const char *name, Decl *decl) {
    return entity_new(ENTITY_ENUM_CONST, name, decl);
}

Entity *entity_get(const char *name) {
    for (Entity **it = entities; it != buf_end(entities); it++) {
        Entity *entity = *it;
        if (entity->name == name) {
            return entity;
        }
    }
    return NULL;
}

Entity *entity_install_decl(Decl *decl) {
    Entity *entity = entity_decl(decl);
    buf_push(entities, entity);
    if (decl->kind == DECL_ENUM) {
        for (size_t i = 0; i < decl->enum_decl.num_items; i++) {
            buf_push(entities, entity_enum_const(decl->enum_decl.items[i].name, decl));
        }
    }
    return entity;
}

Entity *entity_install_type(const char *name, Type *type) {
    Entity *entity = entity_new(ENTITY_TYPE, name, NULL);
    entity->state = ENTITY_RESOLVED;
    entity->type = type;
    buf_push(entities, entity);
    return entity;
}

typedef struct ResolvedExpr {
    Type *type;
    bool is_lvalue;
    bool is_const;
    int64_t val;
} ResolvedExpr;

ResolvedExpr resolved_null;

ResolvedExpr resolved_rvalue(Type *type) {
    return (ResolvedExpr){
        .type = type,
    };
}

ResolvedExpr resolved_lvalue(Type *type) {
    return (ResolvedExpr){
        .type = type,
        .is_lvalue = true,
    };
}

ResolvedExpr resolved_const(int64_t val) {
    return (ResolvedExpr){
        .type = type_int,
        .is_const = true,
        .val = val,
    };
}

Entity *resolve_name(const char *name);
int64_t resolve_const_expr(Expr *expr);
ResolvedExpr resolve_expr(Expr *expr);
ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type);

Type *resolve_typespec(Typespec *typespec) {
    switch (typespec->kind) {
    case TYPESPEC_NAME: {
        Entity *entity = resolve_name(typespec->name);
        if (entity->kind != ENTITY_TYPE) {
            fatal("%s must denote a type", typespec->name);
            return NULL;
        }
        return entity->type;
    }
    case TYPESPEC_PTR:
        return type_ptr(resolve_typespec(typespec->ptr.elem));
    case TYPESPEC_ARRAY:
        return type_array(resolve_typespec(typespec->array.elem), resolve_const_expr(typespec->array.size));
    case TYPESPEC_FUNC: {
        Type **args = NULL;
        for (size_t i = 0; i < typespec->func.num_args; i++) {
            buf_push(args, resolve_typespec(typespec->func.args[i]));
        }
        Type *ret = type_void;
        if (typespec->func.ret) {
            ret = resolve_typespec(typespec->func.ret);
        }
        return type_func(args, buf_len(args), ret);
    }
    default:
        assert(0);
        return NULL;
    }
}

Entity **ordered_entities;

void complete_type(Type *type) {
    if (type->kind == TYPE_COMPLETING) {
        fatal("Type completion cycle");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }
    type->kind = TYPE_COMPLETING;
    Decl *decl = type->entity->decl;
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
    buf_push(ordered_entities, type->entity);
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
        ResolvedExpr result = resolve_expected_expr(decl->var.expr, type);
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
    ResolvedExpr result = resolve_expr(decl->const_decl.expr);
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

void resolve_entity(Entity *entity) {
    if (entity->state == ENTITY_RESOLVED) {
        return;
    } else if (entity->state == ENTITY_RESOLVING) {
        fatal("Cyclic dependency");
        return;
    }
    assert(entity->state == ENTITY_UNRESOLVED);
    entity->state = ENTITY_RESOLVING;
    switch (entity->kind) {
    case ENTITY_TYPE:
        entity->type = resolve_decl_type(entity->decl);
        break;
    case ENTITY_VAR:
        entity->type = resolve_decl_var(entity->decl);
        break;
    case ENTITY_CONST:
        entity->type = resolve_decl_const(entity->decl, &entity->val);
        break;
    case ENTITY_FUNC:
        entity->type = resolve_decl_func(entity->decl);
        break;
    default:
        assert(0);
        break;
    }
    entity->state = ENTITY_RESOLVED;
    buf_push(ordered_entities, entity);
}

void complete_entity(Entity *entity) {
    resolve_entity(entity);
    if (entity->kind == ENTITY_TYPE) {
        complete_type(entity->type);
    }
}

Entity *resolve_name(const char *name) {
    Entity *entity = entity_get(name);
    if (!entity) {
        fatal("Non-existent name");
        return NULL;
    }
    resolve_entity(entity);
    return entity;
}

ResolvedExpr resolve_expr_field(Expr *expr) {
    assert(expr->kind == EXPR_FIELD);
    ResolvedExpr left = resolve_expr(expr->field.expr);
    Type *type = left.type;
    complete_type(type);
    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
        fatal("Can only access fields on aggregate types");
        return resolved_null;
    }
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        TypeField field = type->aggregate.fields[i];
        if (field.name == expr->field.name) {
            return left.is_lvalue ? resolved_lvalue(field.type) : resolved_rvalue(field.type);
        }
    }
    fatal("No field named '%s'", expr->field.name);
    return resolved_null;
}

ResolvedExpr ptr_decay(ResolvedExpr expr) {
    if (expr.type->kind == TYPE_ARRAY) {
        return resolved_rvalue(type_ptr(expr.type->array.elem));
    } else {
        return expr;
    }
}

ResolvedExpr resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    Entity *entity = resolve_name(expr->name);
    if (entity->kind == ENTITY_VAR) {
        return resolved_lvalue(entity->type);
    } else if (entity->kind == ENTITY_CONST) {
        return resolved_const(entity->val);
    } else if (entity->kind == ENTITY_FUNC) {
        return resolved_rvalue(entity->type);
    } else {
        fatal("%s must be a var or const", expr->name);
        return resolved_null;
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

ResolvedExpr resolve_expr_unary(Expr *expr) {
    assert(expr->kind == EXPR_UNARY);
    ResolvedExpr operand = resolve_expr(expr->unary.expr);
    Type *type = operand.type;
    switch (expr->unary.op) {
    case TOKEN_MUL:
        operand = ptr_decay(operand);
        if (type->kind != TYPE_PTR) {
            fatal("Cannot deref non-ptr type");
        }
        return resolved_lvalue(type->ptr.elem);
    case TOKEN_AND:
        if (!operand.is_lvalue) {
            fatal("Cannot take address of non-lvalue");
        }
        return resolved_rvalue(type_ptr(type));
    default:
        if (type->kind != TYPE_INT) {
            fatal("Can only use unary %s with ints", token_kind_name(expr->unary.op));
        }
        if (operand.is_const) {
            return resolved_const(eval_int_unary(expr->unary.op, operand.val));
        } else {
            return resolved_rvalue(type);
        }
    }
}

ResolvedExpr resolve_expr_binary(Expr *expr) {
    assert(expr->kind == EXPR_BINARY);
    ResolvedExpr left = resolve_expr(expr->binary.left);
    ResolvedExpr right = resolve_expr(expr->binary.right);
    if (left.type != type_int) {
        fatal("left operand of + must be int");
    }
    if (right.type != left.type)  {
        fatal("left and right operand of + must have same type");
    }
    if (left.is_const && right.is_const) {
        return resolved_const(eval_int_binary(expr->binary.op, left.val, right.val));
    } else {
        return resolved_rvalue(left.type);
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

ResolvedExpr resolve_expr_compound(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_COMPOUND);
    if (!expected_type && !expr->compound.type) {
        fatal("Implicitly typed compound literals used in context without expected type");
    }
    Type *type = NULL;
    if (expr->compound.type) {
        type = resolve_typespec(expr->compound.type);
        if (expected_type && expected_type != type) {
            fatal("Explicit compound literal type does not match expected type");
        }
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
            ResolvedExpr init = resolve_expected_expr(expr->compound.fields[i].init, type->aggregate.fields[index].type);
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
                index = resolve_const_expr(field.index);
            }
            if (index >= type->aggregate.num_fields) {
                fatal("Field initializer in array compound literal out of range");
            }
            ResolvedExpr init = resolve_expected_expr(expr->compound.fields[i].init, type->array.elem);
            if (init.type != type->array.elem) {
                fatal("Compound literal element type mismatch");
            }
            index++;
        }
    }
    return resolved_rvalue(type);
}

ResolvedExpr resolve_expr_call(Expr *expr) {
    assert(expr->kind == EXPR_CALL);
    ResolvedExpr func = resolve_expr(expr->call.expr);
    if (func.type->kind != TYPE_FUNC) {
        fatal("Trying to call non-function value");
    }
    if (expr->call.num_args != func.type->func.num_params) {
        fatal("Tried to call function with wrong number of arguments");
    }
    for (size_t i = 0; i < expr->call.num_args; i++) {
        Type *param_type = func.type->func.params[i];
        ResolvedExpr arg = resolve_expected_expr(expr->call.args[i], param_type);
        if (arg.type != param_type) {
            fatal("Call argument expression type doesn't match expected param type");
        }
    }
    return resolved_rvalue(func.type->func.ret);
}

ResolvedExpr resolve_expr_ternary(Expr *expr, Type *expected_type) {
    assert(expr->kind == EXPR_TERNARY);
    ResolvedExpr cond = ptr_decay(resolve_expr(expr->ternary.cond));
    if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR) {
        fatal("Ternary cond expression must have type int or ptr");
    }
    ResolvedExpr then_expr = ptr_decay(resolve_expected_expr(expr->ternary.then_expr, expected_type));
    ResolvedExpr else_expr = ptr_decay(resolve_expected_expr(expr->ternary.else_expr, expected_type));
    if (then_expr.type != else_expr.type) {
        fatal("Ternary then/else expressions must have matching types");
    }
    if (cond.is_const && then_expr.is_const && else_expr.is_const) {
        return resolved_const(cond.val ? then_expr.val : else_expr.val);
    } else {
        return resolved_rvalue(then_expr.type);
    }
}

ResolvedExpr resolve_expr_index(Expr *expr) {
    assert(expr->kind == EXPR_INDEX);
    ResolvedExpr operand = ptr_decay(resolve_expr(expr->index.expr));
    if (operand.type->kind != TYPE_PTR) {
        fatal("Can only index arrays or pointers");
    }
    ResolvedExpr index = resolve_expr(expr->index.index);
    if (index.type->kind != TYPE_INT) {
        fatal("Index expression must have type int");
    }
    return resolved_lvalue(operand.type->ptr.elem);
}

ResolvedExpr resolve_expr_cast(Expr *expr) {
    assert(expr->kind == EXPR_CAST);
    Type *type = resolve_typespec(expr->cast.type);
    ResolvedExpr result = ptr_decay(resolve_expr(expr->cast.expr));
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
    return resolved_rvalue(type);
}

ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type) {
    switch (expr->kind) {
    case EXPR_INT:
        return resolved_const(expr->int_val);
    case EXPR_FLOAT:
        return resolved_rvalue(type_float);
    case EXPR_STR:
        return resolved_rvalue(type_ptr(type_char));
    case EXPR_NAME:
        return resolve_expr_name(expr);
    case EXPR_CAST:
        return resolve_expr_cast(expr);
    case EXPR_CALL:
        return resolve_expr_call(expr);
    case EXPR_INDEX:
        return resolve_expr_index(expr);
    case EXPR_FIELD:
        return resolve_expr_field(expr);
    case EXPR_COMPOUND:
        return resolve_expr_compound(expr, expected_type);
    case EXPR_UNARY:
        return resolve_expr_unary(expr);
    case EXPR_BINARY:
        return resolve_expr_binary(expr);
    case EXPR_TERNARY:
        return resolve_expr_ternary(expr, expected_type);
    case EXPR_SIZEOF_EXPR: {
        ResolvedExpr result = resolve_expr(expr->sizeof_expr);
        Type *type = result.type;
        complete_type(type);
        return resolved_const(type_sizeof(type));
    }
    case EXPR_SIZEOF_TYPE: {
        Type *type = resolve_typespec(expr->sizeof_type);
        complete_type(type);
        return resolved_const(type_sizeof(type));
    }
    default:
        assert(0);
        return resolved_null;
    }
}

ResolvedExpr resolve_expr(Expr *expr) {
    return resolve_expected_expr(expr, NULL);
}

int64_t resolve_const_expr(Expr *expr) {
    ResolvedExpr result = resolve_expr(expr);
    if (!result.is_const) {
        fatal("Expected constant expression");
    }
    return result.val;
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

    entity_install_type(str_intern("void"), type_void);
    entity_install_type(str_intern("char"), type_char);
    entity_install_type(str_intern("int"), type_int);

    const char *code[] = {
        "union IntOrPtr { i: int; p: int*; }",
        "var u1 = IntOrPtr{i = 42}",
        "var u2 = IntOrPtr{p = cast(int*, 42)}"
        "var a: int[256] = {1, 2, ['a'] = 42}",
        "struct Vector { x, y: int; }",
        "func add(v: Vector, w: Vector): Vector { return {v.x + w.x, v.y + w.y}; }",
        "var v: Vector = 0 ? {1,2} : {3,4}",
        /*
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
*/
//        "const n = sizeof(x)",
//        "var x: T",
//        "struct T { s: S*; }",
//        "struct S { t: T[n]; }",
    };
    for (size_t i = 0; i < sizeof(code)/sizeof(*code); i++) {
        init_stream(code[i]);
        Decl *decl = parse_decl();
        entity_install_decl(decl);
    }
    for (Entity **it = entities; it != buf_end(entities); it++) {
        Entity *entity = *it;
        complete_entity(entity);
    }
    for (Entity **it = ordered_entities; it != buf_end(ordered_entities); it++) {
        Entity *entity = *it;
        if (entity->decl) {
            print_decl(entity->decl);
        } else {
            printf("%s", entity->name);
        }
        printf("\n");
    }
}
