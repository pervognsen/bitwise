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
    TYPE_TUPLE,
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
    size_t padding;
    Sym *sym;
    Type *base;
    int typeid;
    bool nonmodifiable;
    union {
        struct {
            size_t num_elems;
            bool incomplete_elems;
        };
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            bool intrinsic;
            Type **params;
            size_t num_params;
            bool has_varargs;
            Type *varargs_type;
            Type *ret;
        } func;
    };
};

typedef struct TypeMetrics {
    size_t size;
    size_t align;
    bool sign;
    unsigned long long max;
} TypeMetrics;

TypeMetrics *type_metrics;

Type *type_void = &(Type){TYPE_VOID};
Type *type_bool = &(Type){TYPE_BOOL};
Type *type_char = &(Type){TYPE_CHAR};
Type *type_uchar = &(Type){TYPE_UCHAR};
Type *type_schar = &(Type){TYPE_SCHAR};
Type *type_short = &(Type){TYPE_SHORT};
Type *type_ushort = &(Type){TYPE_USHORT};
Type *type_int = &(Type){TYPE_INT};
Type *type_uint = &(Type){TYPE_UINT};
Type *type_long = &(Type){TYPE_LONG};
Type *type_ulong = &(Type){TYPE_ULONG};
Type *type_llong = &(Type){TYPE_LLONG};
Type *type_ullong = &(Type){TYPE_ULLONG};
Type *type_float = &(Type){TYPE_FLOAT};
Type *type_double = &(Type){TYPE_DOUBLE};

Type *type_char_ptr;
Type *type_alloc_func;

int next_typeid = 1;

Type *type_uintptr;
Type *type_usize;
Type *type_ssize;

Type *type_any;

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
    return type && type->kind == TYPE_PTR;
}

bool is_func_type(Type *type) {
    return type && type->kind == TYPE_FUNC;
}

bool is_ptr_like_type(Type *type) {
    return type && (type->kind == TYPE_PTR || type->kind == TYPE_FUNC);
}

bool is_const_type(Type *type) {
    return type && type->kind == TYPE_CONST;
}

bool is_array_type(Type *type) {
    return type && type->kind == TYPE_ARRAY;
}

bool is_incomplete_array_type(Type *type) {
    return type && is_array_type(type) && type->incomplete_elems;
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
    return type->kind == TYPE_STRUCT || type->kind == TYPE_UNION || type->kind == TYPE_TUPLE;
}

bool is_signed_type(Type *type) {
    switch (type->kind) {
    case TYPE_CHAR:
        return type_metrics[TYPE_CHAR].sign;
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

size_t type_padding(Type *type) {
    assert(type->kind > TYPE_COMPLETING);
    return type->padding;
}

Map cached_ptr_types;

Type *type_ptr(Type *base) {
    Type *type = map_get(&cached_ptr_types, base);
    if (!type) {
        type = type_alloc(TYPE_PTR);
        type->size = type_metrics[TYPE_PTR].size;
        type->align = type_metrics[TYPE_PTR].align;
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

Type *qualify_type(Type *type, Type *qual) {
    type = unqualify_type(type);
    while (qual->kind == TYPE_CONST) {
        type = type_const(type);
        qual = qual->base;
    }
    return type;
}

typedef struct CachedArrayType {
    Type *type;
    struct CachedArrayType *next;
} CachedArrayType;

Map cached_array_types;

Type *type_array(Type *base, size_t num_elems, bool incomplete_elems) {
    uint64_t hash = hash_mix(hash_ptr(base), hash_uint64(num_elems));
    uint64_t key = hash ? hash : 1;
    CachedArrayType *cached = map_get_from_uint64(&cached_array_types, key);
    if (!incomplete_elems) {
        for (CachedArrayType *it = cached; it; it = it->next) {
            Type *type = it->type;
            if (type->base == base && type->num_elems == num_elems) {
                return type;
            }
        }
    }
    complete_type(base);
    Type *type = type_alloc(TYPE_ARRAY);
    type->nonmodifiable = base->nonmodifiable;
    type->size = num_elems * type_sizeof(base);
    type->align = type_alignof(base);
    type->base = base;
    type->num_elems = num_elems;
    type->incomplete_elems = incomplete_elems;
    if (!incomplete_elems) {
        CachedArrayType *new_cached = xmalloc(sizeof(CachedArrayType));
        new_cached->type = type;
        new_cached->next = cached;
        map_put_from_uint64(&cached_array_types, key, new_cached);
    }
    return type;
}

typedef struct TypeLink {
    Type *type;
    struct TypeLink *next;
} TypeLink;

Map cached_func_types;

Type *type_func(Type **params, size_t num_params, Type *ret, bool intrinsic, bool has_varargs, Type *varargs_type) {
    size_t params_size = num_params * sizeof(*params);
    uint64_t hash = hash_mix(hash_bytes(params, params_size), hash_ptr(ret));
    uint64_t key = hash ? hash : 1;
    TypeLink *cached = map_get_from_uint64(&cached_func_types, key);
    for (TypeLink *it = cached; it; it = it->next) {
        Type *type = it->type;
        if (type->func.num_params == num_params && type->func.ret == ret && type->func.intrinsic == intrinsic && type->func.has_varargs == has_varargs && type->func.varargs_type == varargs_type) {
            if (memcmp(type->func.params, params, params_size) == 0) {
                return type;
            }
        }
    }
    Type *type = type_alloc(TYPE_FUNC);
    type->size = type_metrics[TYPE_PTR].size;
    type->align = type_metrics[TYPE_PTR].align;
    type->func.params = memdup(params, params_size);
    type->func.num_params = num_params;
    type->func.intrinsic = intrinsic;
    type->func.has_varargs = has_varargs;
    type->func.varargs_type = varargs_type;
    type->func.ret = ret;
    TypeLink *new_cached = xmalloc(sizeof(TypeLink));
    new_cached->type = type;
    new_cached->next = cached;
    map_put_from_uint64(&cached_func_types, key, new_cached);
    return type;
}

bool has_duplicate_fields(Type *type) {
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        for (size_t j = i+1; j < type->aggregate.num_fields; j++) {
            if (type->aggregate.fields[i].name == type->aggregate.fields[j].name) {
                return true;
            }
        }
    }
    return false;
}

void add_type_fields(TypeField **fields, Type *type, size_t offset) {
    assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        TypeField *field = &type->aggregate.fields[i];
        buf_push(*fields, (TypeField){field->name, field->type, field->offset + offset});
    }
}

void type_complete_struct(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_STRUCT;
    type->size = 0;
    type->align = 0;
    bool nonmodifiable = false;
    size_t field_sizes = 0;
    TypeField *new_fields = NULL;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(IS_POW2(type_alignof(it->type)));
        if (it->name) {
            it->offset = type->size;
            buf_push(new_fields, *it);
        } else {
            add_type_fields(&new_fields, it->type, type->size);
        }
        field_sizes += type_sizeof(it->type);
        type->align = MAX(type->align, type_alignof(it->type));
        type->size = type_sizeof(it->type) + ALIGN_UP(type->size, type_alignof(it->type));
        nonmodifiable = it->type->nonmodifiable || nonmodifiable;
    }
    type->size = ALIGN_UP(type->size, type->align);
    type->padding = type->size - field_sizes;
    type->aggregate.fields = new_fields;
    type->aggregate.num_fields = buf_len(new_fields);
    type->nonmodifiable = nonmodifiable;
}

void type_complete_union(Type *type, TypeField *fields, size_t num_fields) {
    assert(type->kind == TYPE_COMPLETING);
    type->kind = TYPE_UNION;
    type->size = 0;
    type->align = 0;
    bool nonmodifiable = false;
    TypeField *new_fields = NULL;
    for (TypeField *it = fields; it != fields + num_fields; it++) {
        assert(it->type->kind > TYPE_COMPLETING);
        if (it->name) {
            it->offset = 0;
            buf_push(new_fields, *it);
        } else {
            add_type_fields(&new_fields, it->type, 0);
        }
        type->size = MAX(type->size, type_sizeof(it->type));
        type->align = MAX(type->align, type_alignof(it->type));
        nonmodifiable = it->type->nonmodifiable || nonmodifiable;
    }
    type->size = ALIGN_UP(type->size, type->align);
    type->aggregate.fields = new_fields;
    type->aggregate.num_fields = buf_len(new_fields);
    type->nonmodifiable = nonmodifiable;
}

void type_complete_tuple(Type *type, Type **fields, size_t num_fields) {
    type->kind = TYPE_TUPLE;
    type->size = 0;
    type->align = 0;
    bool nonmodifiable = false;
    size_t elem_sizes = 0;
    TypeField *new_fields = NULL;
    for (size_t i = 0; i < num_fields; i++) {
        Type *field = fields[i];
        complete_type(fields[i]);
        assert(IS_POW2(type_alignof(field)));
        char name[64];
        snprintf(name, sizeof(name), "_%d", (int)i);
        TypeField new_field = {
            .name = str_intern(name),
            .type = fields[i],
            .offset = type->size,
        };
        buf_push(new_fields, new_field);
        elem_sizes += type_sizeof(field);
        type->align = MAX(type->align, type_alignof(field));
        type->size = type_sizeof(field) + ALIGN_UP(type->size, type_alignof(field));
        nonmodifiable = field->nonmodifiable || nonmodifiable;
    }
    type->size = ALIGN_UP(type->size, type->align);
    type->padding = type->size - elem_sizes;
    type->aggregate.fields = new_fields;
    type->aggregate.num_fields = buf_len(new_fields);
    type->nonmodifiable = nonmodifiable;
}

Type *type_incomplete(Sym *sym) {
    Type *type = type_alloc(TYPE_INCOMPLETE);
    type->sym = sym;
    return type;
}

Type *type_enum(Sym *sym, Type *base) {
    Type *type = type_alloc(TYPE_ENUM);
    type->sym = sym;
    type->base = base;
    type->size = type_int->size;
    type->align = type_int->align;
    return type;
}

void init_builtin_type(Type *type) {
    type->typeid = next_typeid++;
    register_typeid(type);
    type->size = type_metrics[type->kind].size;
    type->align = type_metrics[type->kind].align;
}

void init_builtin_types(void) {
    init_builtin_type(type_void);
    init_builtin_type(type_bool);
    init_builtin_type(type_char);
    init_builtin_type(type_uchar);
    init_builtin_type(type_schar);
    init_builtin_type(type_short);
    init_builtin_type(type_ushort);
    init_builtin_type(type_int);
    init_builtin_type(type_uint);
    init_builtin_type(type_long);
    init_builtin_type(type_ulong);
    init_builtin_type(type_llong);
    init_builtin_type(type_ullong);
    init_builtin_type(type_float);
    init_builtin_type(type_double);

    type_char_ptr = type_ptr(type_char);

    type_alloc_func = type_func((Type*[]){type_usize, type_usize}, 2, type_ptr(type_void), false, false, type_void);
}

int aggregate_item_field_index(Type *type, const char *name) {
    assert(is_aggregate_type(type));
    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        if (type->aggregate.fields[i].name == name) {
            return (int)i;
        }
    }
    return -1;
}

Type *aggregate_item_field_type_from_index(Type *type, int index) {
    assert(is_aggregate_type(type));
    assert(0 <= index && index < (int)type->aggregate.num_fields);
    return type->aggregate.fields[index].type;
}

Type *aggregate_item_field_type_from_name(Type *type, const char *name) {
    assert(is_aggregate_type(type));
    int index = aggregate_item_field_index(type, name);
    if (index < 0) {
        return NULL;
    }
    return aggregate_item_field_type_from_index(type, index);
}

Map cached_tuple_types;
Type **tuple_types;

Type *type_tuple(Type **fields, size_t num_fields) {
    size_t fields_size = num_fields * sizeof(*fields);
    uint64_t hash = hash_bytes(fields, fields_size);
    uint64_t key = hash ? hash : 1;
    TypeLink *cached = map_get_from_uint64(&cached_tuple_types, key);
    for (TypeLink *it = cached; it; it = it->next) {
        Type *cached_type = it->type;
        if (cached_type->aggregate.num_fields == num_fields) {
            for (size_t i = 0; i < num_fields; i++) {
                if (cached_type->aggregate.fields[i].type != fields[i]) {
                    goto next;
                }
            }
            return cached_type;
        }
        next: ;
    }
    Type *type = type_alloc(TYPE_TUPLE);
    type_complete_tuple(type, fields, num_fields);
    TypeLink *new_cached = xmalloc(sizeof(TypeLink));
    new_cached->type = type;
    new_cached->next = cached;
    map_put_from_uint64(&cached_tuple_types, key, new_cached);
    buf_push(tuple_types, type);
    return type;
}

Type *unqualify_ptr_type(Type *type) {
    if (type->kind == TYPE_PTR) {
        type = type_ptr(unqualify_type(type->base));
    }
    return type;
}
