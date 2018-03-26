// Please don't file bugs for this code yet. It's just a scratchpad for now.

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
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

Type type_int_val = {TYPE_INT, 4};
Type type_float_val = {TYPE_FLOAT, 4};

Type *type_int = &type_int_val;
Type *type_float = &type_float_val;
const size_t PTR_SIZE = 8;

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
    type->size = size * elem->size;
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
    type->func.params = memdup(params, num_params * sizeof(*params));
    type->func.num_params = num_params;
    type->func.ret = ret;
    buf_push(cached_func_types, (CachedFuncType){params, num_params, ret, type});
    return type;
}

void type_complete_struct(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_STRUCT;
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(it->type->kind > TYPE_COMPLETING);
        // TODO: Alignment, etc.
        type->size += it->type->size;
    }
    type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
    type->aggregate.num_fields = num_fields;
}

void type_complete_union(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_UNION;
    type->size = 0;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(it->type->kind > TYPE_COMPLETING);
        type->size = MAX(type->size, it->type->size);
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
int64_t resolve_int_const_expr(Expr *expr);
ResolvedExpr resolve_expr(Expr *expr);

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
        return type_array(resolve_typespec(typespec->array.elem), resolve_int_const_expr(typespec->array.size));
    case TYPESPEC_FUNC: {
        Type **args = NULL;
        for (size_t i = 0; i < typespec->func.num_args; i++) {
            buf_push(args, resolve_typespec(typespec->func.args[i]));
        }
        return type_func(args, buf_len(args), resolve_typespec(typespec->func.ret));
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
        ResolvedExpr result = resolve_expr(decl->var.expr);
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
    *val = result.val;
    return result.type;
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
    default:
        assert(0);
        break;
    }
    entity->state = ENTITY_RESOLVED;
    buf_push(ordered_entities, entity);
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
    fatal("No field named %s on type", expr->field.name);
    return resolved_null;
}

ResolvedExpr resolve_expr_name(Expr *expr) {
    assert(expr->kind == EXPR_NAME);
    Entity *entity = resolve_name(expr->name);
    if (entity->kind == ENTITY_VAR) {
        return resolved_lvalue(entity->type);
    } else if (entity->kind == ENTITY_CONST) {
        return resolved_const(entity->val);
    } else {
        fatal("%s must denote a var or const", expr->name);
        return resolved_null;
    }
}

ResolvedExpr resolve_expr_unary(Expr *expr) {
    assert(expr->kind == EXPR_UNARY);
    ResolvedExpr operand = resolve_expr(expr->unary.expr);
    Type *type = operand.type;
    switch (expr->unary.op) {
    case TOKEN_MUL:
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
        assert(0);
        return resolved_null;
    }
}

ResolvedExpr resolve_expr_binary(Expr *expr) {
    assert(expr->kind == EXPR_BINARY);
    assert(expr->binary.op == TOKEN_ADD);
    ResolvedExpr left = resolve_expr(expr->binary.left);
    ResolvedExpr right = resolve_expr(expr->binary.right);
    if (left.type != type_int) {
        fatal("left operand of + is not int");
    }
    if (right.type != left.type)  {
        fatal("left and right operand of + must have same type");
    }
    if (left.is_const && right.is_const) {
        return resolved_const(left.val + right.val);
    } else {
        return resolved_rvalue(left.type);
    }
}

ResolvedExpr resolve_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_INT:
        return resolved_const(expr->int_val);
    case EXPR_NAME:
        return resolve_expr_name(expr);
    case EXPR_FIELD:
        return resolve_expr_field(expr);
    case EXPR_UNARY:
        return resolve_expr_unary(expr);
    case EXPR_BINARY:
        return resolve_expr_binary(expr);
    case EXPR_SIZEOF_EXPR: {
        ResolvedExpr result = resolve_expr(expr->sizeof_expr);
        Type *type = result.type;
        complete_type(type);
        return resolved_const(type->size);
    }
    case EXPR_SIZEOF_TYPE: {
        Type *type = resolve_typespec(expr->sizeof_type);
        complete_type(type);
        return resolved_const(type->size);
    }
    default:
        assert(0);
        return resolved_null;
    }
}

int64_t resolve_int_const_expr(Expr *expr) {
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

    const char *int_name = str_intern("int");
    entity_install_type(int_name, type_int);
    const char *code[] = {
        "const n = 1+sizeof(p)",
        "var p: T*",
        "struct T { i: int[sizeof(&p)]; }",
        "var t: T",
        "typedef S = int[n+m]",
        "const m = sizeof(t.i)",
        "var q = &p",
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
        resolve_entity(entity);
        if (entity->kind == ENTITY_TYPE) {
            complete_type(entity->type);
        }
    }
    for (Entity **it = ordered_entities; it != buf_end(ordered_entities); it++) {
        Entity *entity = *it;
        printf("%s\n", entity->name);
    }
}
