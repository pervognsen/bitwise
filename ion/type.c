typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_CHAR,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LLONG,
    TYPE_ULLONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_PTR,
    TYPE_FUNC,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_CONST,
    NUM_TYPE_KINDS,
} TypeKind;

typedef struct Type Type;
typedef struct Sym Sym;

typedef struct TypeField {
    const char *name;
    Type *type;
    size_t offset;
} TypeField;

struct Type {
    TypeKind kind;
    size_t size;
    size_t align;
    Sym *sym;
    Type *base;
    bool nonmodifiable;
    union {
        size_t num_elems;
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            Type **params;
            size_t num_params;
            bool variadic;
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
Type *type_bool = &(Type){TYPE_BOOL, 1, 1};
Type *type_char = &(Type){TYPE_CHAR, 1, 1};
Type *type_uchar = &(Type){TYPE_UCHAR, 1, 1};
Type *type_schar = &(Type){TYPE_SCHAR, 1, 1};
Type *type_short = &(Type){TYPE_SHORT, 2, 2};
Type *type_ushort = &(Type){TYPE_USHORT, 2, 2};
Type *type_int = &(Type){TYPE_INT, 4, 4};
Type *type_uint = &(Type){TYPE_UINT, 4, 4};
Type *type_long = &(Type){TYPE_LONG, 4, 4}; // 4 on 64-bit windows, 8 on 64-bit linux, probably factor this out to the backend
Type *type_ulong = &(Type){TYPE_ULONG, 4, 4};
Type *type_llong = &(Type){TYPE_LLONG, 8, 8};
Type *type_ullong = &(Type){TYPE_ULLONG, 8, 8};
Type *type_float = &(Type){TYPE_FLOAT, 4, 4};
Type *type_double = &(Type){TYPE_DOUBLE, 8, 8};

#define type_usize type_ullong
#define type_ssize type_llong

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGN = 8;

bool is_ptr_type(Type *type) {
    return type->kind == TYPE_PTR;
}

bool is_array_type(Type *type) {
    return type->kind == TYPE_ARRAY;
}

bool is_incomplete_array_type(Type *type) {
    return is_array_type(type) && type->num_elems == 0;
}

bool is_integer_type(Type *type) {
    return TYPE_BOOL <= type->kind && type->kind <= TYPE_ULLONG;
}

bool is_floating_type(Type *type) {
    return TYPE_FLOAT <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_arithmetic_type(Type *type) {
    return TYPE_BOOL && type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_scalar_type(Type *type) {
    return TYPE_BOOL <= type->kind && type->kind <= TYPE_FUNC;
}

bool is_signed_type(Type *type) {
    switch (type->kind) {
        // TODO: TYPE_CHAR signedness is platform independent, needs to factor into backend
    case TYPE_SCHAR:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_LLONG:
        return true;
    default:
        return false;
    }
}

const char *type_names[NUM_TYPE_KINDS] = {
    [TYPE_VOID] = "void",
    [TYPE_BOOL] = "bool",
    [TYPE_CHAR] = "char",
    [TYPE_SCHAR] = "schar",
    [TYPE_UCHAR] = "uchar",
    [TYPE_SHORT] = "short",
    [TYPE_USHORT] = "ushort",
    [TYPE_INT] = "int",
    [TYPE_UINT] = "uint",
    [TYPE_LONG] = "long",
    [TYPE_ULONG] = "ulong",
    [TYPE_LLONG] = "llong",
    [TYPE_ULLONG] = "ullong",
    [TYPE_FLOAT] = "float",
    [TYPE_DOUBLE] = "double",
};

int type_ranks[NUM_TYPE_KINDS] = {
    [TYPE_BOOL] = 1,
    [TYPE_CHAR] = 2,
    [TYPE_SCHAR] = 2,
    [TYPE_UCHAR] = 2,
    [TYPE_SHORT] = 3,
    [TYPE_USHORT] = 3,
    [TYPE_INT] = 4,
    [TYPE_UINT] = 4,
    [TYPE_LONG] = 5,
    [TYPE_ULONG] = 5,
    [TYPE_LLONG] = 6,
    [TYPE_ULLONG] = 6,
};

int type_rank(Type *type) {
    int rank = type_ranks[type->kind];
    assert(rank != 0);
    return rank;
}

Type *unsigned_type(Type *type) {
    switch (type->kind) {
    case TYPE_BOOL:
        return type_bool;
    case TYPE_CHAR:
    case TYPE_SCHAR:
    case TYPE_UCHAR:
        return type_uchar;
    case TYPE_SHORT:
    case TYPE_USHORT:
        return type_ushort;
    case TYPE_INT:
    case TYPE_UINT:
        return type_uint;
    case TYPE_LONG:
    case TYPE_ULONG:
        return type_ulong;
    case TYPE_LLONG:
    case TYPE_ULLONG:
        return type_ullong;
    default:
        assert(0);
        return NULL;
    }
}

size_t type_sizeof(Type *type) {
    assert(type->kind > TYPE_COMPLETING);
    assert(type->size != 0);
    return type->size;
}

size_t type_alignof(Type *type) {
    assert(type->kind > TYPE_COMPLETING);
    return type->align;
}

Map cached_ptr_types;

Type *type_ptr(Type *base) {
    Type *type = map_get(&cached_ptr_types, base);
    if (!type) {
        type = type_alloc(TYPE_PTR);
        type->size = PTR_SIZE;
        type->align = PTR_ALIGN;
        type->base = base;
        map_put(&cached_ptr_types, base, type);
    }
    return type;
}

Map cached_const_types;

Type *type_const(Type *base) {
    if (base->kind == TYPE_CONST) {
        return base;
    }
    Type *type = map_get(&cached_const_types, base);
    if (!type) {
        complete_type(base);
        type = type_alloc(TYPE_CONST);
        type->nonmodifiable = true;
        type->size = base->size;
        type->align = base->align;
        type->base = base;
        map_put(&cached_const_types, base, type);
    }
    return type;
}

Type *unqualify_type(Type *type) {
    if (type->kind == TYPE_CONST) {
        return type->base;
    } else {
        return type;
    }
}

typedef struct CachedArrayType {
    Type *elem;
    size_t num_elems;
    Type *array;
} CachedArrayType;

CachedArrayType *cached_array_types;

Type *type_array(Type *elem, size_t num_elems) {
    for (CachedArrayType *it = cached_array_types; it != buf_end(cached_array_types); it++) {
        if (it->elem == elem && it->num_elems == num_elems) {
            return it->array;
        }
    }
    complete_type(elem);
    Type *type = type_alloc(TYPE_ARRAY);
    type->nonmodifiable = elem->nonmodifiable;
    type->size = num_elems * type_sizeof(elem);
    type->align = type_alignof(elem);
    type->base = elem;
    type->num_elems = num_elems;
    buf_push(cached_array_types, (CachedArrayType){elem, num_elems, type});
    return type;
}

typedef struct CachedFuncType {
    Type **params;
    size_t num_params;
    bool variadic;
    Type *ret;
    Type *func;
} CachedFuncType;

CachedFuncType *cached_func_types;

Type *type_func(Type **params, size_t num_params, Type *ret, bool variadic) {
    for (CachedFuncType *it = cached_func_types; it != buf_end(cached_func_types); it++) {
        if (it->num_params == num_params && it->ret == ret && it->variadic == variadic) {
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
    type->func.variadic = variadic;
    type->func.ret = ret;
    buf_push(cached_func_types, (CachedFuncType){params, num_params, ret, type});
    return type;
}

bool has_duplicate_fields(TypeField *fields, size_t num_fields) {
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
    bool nonmodifiable = false;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(IS_POW2(type_alignof(it->type)));
        it->offset = type->size;
        type->size = type_sizeof(it->type) + ALIGN_UP(type->size, type_alignof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
        nonmodifiable = it->type->nonmodifiable || nonmodifiable;
    }
    type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
    type->aggregate.num_fields = num_fields;
    type->nonmodifiable = nonmodifiable;
}

void type_complete_union(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_UNION;
    type->size = 0;
    type->align = 0;
    bool nonmodifiable = false;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(it->type->kind > TYPE_COMPLETING);
        it->offset = 0;
        type->size = MAX(type->size, type_sizeof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
        nonmodifiable = it->type->nonmodifiable || nonmodifiable;
    }
    type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
    type->aggregate.num_fields = num_fields;
    type->nonmodifiable = nonmodifiable;
}

Type *type_incomplete(Sym *sym) {
    Type *type = type_alloc(TYPE_INCOMPLETE);
    type->sym = sym;
    return type;
}
