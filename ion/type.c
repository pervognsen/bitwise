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
    TYPE_ENUM,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_PTR,
    TYPE_FUNC,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
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
    int typeid;
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
            bool has_varargs;
            Type *ret;
        } func;
    };
};

Type *type_void = &(Type){TYPE_VOID, .size = 0, .align = 0, .typeid = 1};
Type *type_bool = &(Type){TYPE_BOOL, .size = 1, .align = 1, .typeid = 2};
Type *type_char = &(Type){TYPE_CHAR, .size = 1, .align = 1, .typeid = 3};
Type *type_uchar = &(Type){TYPE_UCHAR, .size = 1, .align = 1, .typeid = 4};
Type *type_schar = &(Type){TYPE_SCHAR, .size = 1, .align = 1, .typeid = 5};
Type *type_short = &(Type){TYPE_SHORT, .size = 2, .align = 2, .typeid = 6};
Type *type_ushort = &(Type){TYPE_USHORT, .size = 2, .align = 2, .typeid = 7};
Type *type_int = &(Type){TYPE_INT, .size = 4, .align = 4, .typeid = 8};
Type *type_uint = &(Type){TYPE_UINT, .size = 4, .align = 4, .typeid = 9};
Type *type_long = &(Type){TYPE_LONG, .size = 4, .align = 4, .typeid = 10}; // 4 on 64-bit windows, 8 on 64-bit linux, probably factor this out to the backend
Type *type_ulong = &(Type){TYPE_ULONG, .size = 4, .align = 4, .typeid = 11};
Type *type_llong = &(Type){TYPE_LLONG, .size = 8, .align = 8, .typeid = 12};
Type *type_ullong = &(Type){TYPE_ULLONG, .size = 8, .align = 8, .typeid = 13};
Type *type_float = &(Type){TYPE_FLOAT, .size = 4, .align = 4, .typeid = 14};
Type *type_double = &(Type){TYPE_DOUBLE, .size = 8, .align = 8, .typeid = 15};

int next_typeid = 16;

#define type_uintptr type_ullong
#define type_usize type_ullong
#define type_ssize type_llong

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGN = 8;

void complete_type(Type *type);

Map typeid_map;

Type *get_type_from_typeid(int typeid) {
    if (typeid == 0) {
        return NULL;
    }
    return map_get(&typeid_map, (void *)(uintptr_t)typeid);
}

void register_typeid(Type *type) {
    map_put(&typeid_map, (void *)(uintptr_t)type->typeid, type);
}

Type *type_alloc(TypeKind kind) {
    Type *type = xcalloc(1, sizeof(Type));
    type->kind = kind;
    type->typeid = next_typeid++;
    register_typeid(type);
    return type;
}

bool is_ptr_type(Type *type) {
    return type->kind == TYPE_PTR;
}

bool is_const_type(Type *type) {
    return type->kind == TYPE_CONST;
}

bool is_array_type(Type *type) {
    return type->kind == TYPE_ARRAY;
}

bool is_incomplete_array_type(Type *type) {
    return is_array_type(type) && type->num_elems == 0;
}

bool is_integer_type(Type *type) {
    return TYPE_BOOL <= type->kind && type->kind <= TYPE_ENUM;
}

bool is_floating_type(Type *type) {
    return TYPE_FLOAT <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_arithmetic_type(Type *type) {
    return TYPE_BOOL <= type->kind && type->kind <= TYPE_DOUBLE;
}

bool is_scalar_type(Type *type) {
    return TYPE_BOOL <= type->kind && type->kind <= TYPE_FUNC;
}

bool is_aggregate_type(Type *type) {
    return type->kind == TYPE_STRUCT || type->kind == TYPE_UNION;
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
    Type *type;
    struct CachedArrayType *next;
} CachedArrayType;

Map cached_array_types;

Type *type_array(Type *base, size_t num_elems) {
    uint64_t hash = hash_mix(hash_ptr(base), hash_uint64(num_elems));
    void *key = (void *)(hash ? hash : 1);
    CachedArrayType *cached = map_get(&cached_array_types, key);
    for (CachedArrayType *it = cached; it; it = it->next) {
        Type *type = it->type;
        if (type->base == base && type->num_elems == num_elems) {
            return type;
        }
    }
    complete_type(base);
    Type *type = type_alloc(TYPE_ARRAY);
    type->nonmodifiable = base->nonmodifiable;
    type->size = num_elems * type_sizeof(base);
    type->align = type_alignof(base);
    type->base = base;
    type->num_elems = num_elems;
    CachedArrayType *new_cached = xmalloc(sizeof(CachedArrayType));
    new_cached->type = type;
    new_cached->next = cached;
    map_put(&cached_array_types, key, new_cached);
    return type;
}

typedef struct CachedFuncType {
    Type *type;
    struct CachedFuncType *next;
} CachedFuncType;

Map cached_func_types;

Type *type_func(Type **params, size_t num_params, Type *ret, bool has_varargs) {
    size_t params_size = num_params * sizeof(*params);
    uint64_t hash = hash_mix(hash_bytes(params, params_size), hash_ptr(ret));
    void *key = (void *)(hash ? hash : 1);
    CachedFuncType *cached = map_get(&cached_func_types, key);
    for (CachedFuncType *it = cached; it; it = it->next) {
        Type *type = it->type;
        if (type->func.num_params == num_params && type->func.ret == ret && type->func.has_varargs == has_varargs) {
            if (memcmp(type->func.params, params, params_size) == 0) {
                return type;
            }
        }
    }
    Type *type = type_alloc(TYPE_FUNC);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->func.params = memdup(params, params_size);
    type->func.num_params = num_params;
    type->func.has_varargs = has_varargs;
    type->func.ret = ret;
    CachedFuncType *new_cached = xmalloc(sizeof(CachedFuncType));
    new_cached->type = type;
    new_cached->next = cached;
    map_put(&cached_func_types, key, new_cached);
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

Type *type_enum(Sym *sym) {
    Type *type = type_alloc(TYPE_ENUM);
    type->sym = sym;
    type->size = type_int->size;
    type->align = type_int->align;
    return type;
}

void init_builtin_types(void) {
    register_typeid(type_void);
    register_typeid(type_bool);
    register_typeid(type_char);
    register_typeid(type_uchar);
    register_typeid(type_schar);
    register_typeid(type_short);
    register_typeid(type_ushort);
    register_typeid(type_int);
    register_typeid(type_uint);
    register_typeid(type_long);
    register_typeid(type_ulong);
    register_typeid(type_llong);
    register_typeid(type_ullong);
    register_typeid(type_float);
    register_typeid(type_double);
}


int aggregate_field_index(Type *type, const char *name) {
    assert(is_aggregate_type(type));
    for (int i = 0; i < type->aggregate.num_fields; i++) {
        if (type->aggregate.fields[i].name == name) {
            return i;
        }
    }
    return -1;
}

Type *aggregate_field_type_from_index(Type *type, int index) {
    assert(is_aggregate_type(type));
    assert(0 <= index && index < type->aggregate.num_fields);
    return type->aggregate.fields[index].type;
}

Type *aggregate_field_type_from_name(Type *type, const char *name) {
    assert(is_aggregate_type(type));
    int index = aggregate_field_index(type, name);
    if (index < 0) {
        return NULL;
    }
    return aggregate_field_type_from_index(type, index);
}
