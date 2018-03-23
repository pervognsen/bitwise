// Please don't file bugs for this code yet. It's just a scratchpad for now.

typedef enum TypeKind {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_FUNC,
} TypeKind;

typedef struct Type Type;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

struct Type {
    TypeKind kind;
    union {
        struct {
            Type *base;
        } ptr;
        struct {
            Type *base;
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

Type *type_alloc(TypeKind kind) {
    Type *t = xcalloc(1, sizeof(Type));
    t->kind = kind;
    return t;
}

Type type_int_val = {TYPE_INT};
Type type_float_val = {TYPE_FLOAT};

Type *type_int = &type_int_val;
Type *type_float = &type_float_val;

typedef struct CachedPtrType {
    Type *base;
    Type *ptr;
} CachedPtrType;

CachedPtrType *cached_ptr_types;

Type *type_ptr(Type *base) {
    for (CachedPtrType *it = cached_ptr_types; it != buf_end(cached_ptr_types); it++) {
        if (it->base == base) {
            return it->ptr;
        }
    }
    Type *t = type_alloc(TYPE_PTR);
    t->ptr.base = base;
    buf_push(cached_ptr_types, (CachedPtrType){base, t});
    return t;
}

typedef struct CachedArrayType {
    Type *base;
    size_t size;
    Type *array;
} CachedArrayType;

CachedArrayType *cached_array_types;

Type *type_array(Type *base, size_t size) {
    for (CachedArrayType *it = cached_array_types; it != buf_end(cached_array_types); it++) {
        if (it->base == base && it->size == size) {
            return it->array;
        }
    }
    Type *t = type_alloc(TYPE_ARRAY);
    t->array.base = base;
    t->array.size = size;
    buf_push(cached_array_types, (CachedArrayType){base, size, t});
    return t;
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
    Type *t = type_alloc(TYPE_FUNC);
    t->func.params = xcalloc(num_params, sizeof(Type *));
    memcpy(t->func.params, params, num_params * sizeof(Type *));
    t->func.num_params = num_params;
    t->func.ret = ret;
    buf_push(cached_func_types, (CachedFuncType){params, num_params, ret, t});
    return t;
}

Type *type_struct(TypeField *fields, size_t num_fields) {
    Type *t = type_alloc(TYPE_STRUCT);
    t->aggregate.fields = xcalloc(num_fields, sizeof(TypeField));
    memcpy(t->aggregate.fields, fields, num_fields * sizeof(TypeField));
    t->aggregate.num_fields = num_fields;
    return t;
}

Type *type_union(TypeField *fields, size_t num_fields) {
    Type *t = type_alloc(TYPE_UNION);
    t->aggregate.fields = xcalloc(num_fields, sizeof(TypeField));
    memcpy(t->aggregate.fields, fields, num_fields * sizeof(TypeField));
    t->aggregate.num_fields = num_fields;
    return t;
}

typedef struct ConstEntity {
    Type *type;
    union {
        uint64_t int_val;
        double float_val;
    };
} ConstEntity;

typedef struct Entity {
    int foo;
} Entity;

typedef enum SymState {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

typedef struct Sym {
    const char *name;
    Decl *decl;
    SymState state;
    Entity *ent;
} Sym;

Sym *syms;

Sym *sym_get(const char *name) {
    for (Sym *it = syms; it != buf_end(syms); it++) {
        if (it->name == name) {
            return it;
        }
    }
    return NULL;
}

void sym_put(Decl *decl) {
    assert(decl->name);
    assert(!sym_get(decl->name));
    buf_push(syms, (Sym){decl->name, decl, SYM_UNRESOLVED});
}

void resolve_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_CONST:
        break;
    }
}

void resolve_sym(Sym *sym) {
    if (sym->state == SYM_RESOLVED) {
        return;
    }
    if (sym->state == SYM_RESOLVING) {
        fatal("Cyclic dependency");
        return;
    }
    resolve_decl(sym->decl);
}

Sym *resolve_name(const char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        fatal("Unknown name");
        return NULL;
    }
    resolve_sym(sym);
    return sym;
}

void resolve_syms() {
    for (Sym *it = syms; it != buf_end(syms); it++) {
        resolve_sym(it);
    }
}

void resolve_test() {
    const char *foo = str_intern("foo");
    assert(sym_get(foo) == NULL);
    Decl *decl = decl_const(foo, expr_int(42));
    sym_put(decl);
    Sym *sym = sym_get(foo);
    assert(sym && sym->decl == decl);

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
}
