typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

typedef enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_POINTER,
} TypespecKind;

typedef struct FuncTypespec {
    size_t num_args;
    Typespec **args;
    Typespec *ret;
} FuncTypespec;

struct Typespec {
    TypespecKind kind;
    struct {
        const char *name;
        FuncTypespec func;
        struct {
            Typespec *base;
            Expr *size;
        };
    };
};

typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPEDEF,
    DECL_FUNC,
} DeclKind;

typedef struct EnumItem {
    const char *name;
    Typespec *type;
} EnumItem;

typedef struct AggregateItem {
    const char **names;
    size_t num_names;
    Typespec *type;
} AggregateItem;

typedef struct FuncParam {
    const char *name;
    Typespec *type;
} FuncParam;

typedef struct FuncDecl {
    FuncParam *params;
    size_t num_params;
    Typespec *ret_type;
} FuncDecl;

struct Decl {
    DeclKind kind;
    const char *name;
    union {
        struct {
            size_t num_enum_items;
            EnumItem *enum_items;
        };
        struct {
            size_t num_aggregate_items;
            AggregateItem *aggregate_items;
        };
        struct {
            Typespec *type;
            Expr *expr;
        };
        FuncDecl func_decl;
    };
};

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CAST,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_COMPOUND,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
} ExprKind;

struct Expr {
    ExprKind kind;
    TokenKind op;
    union {
        // Literals/names
        uint64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
        // Compound literals
        struct {
            Typespec *compound_type;
            size_t num_compound_args;
            Expr **compound_args;
        };
        // Casts
        struct {
            Typespec *cast_type;
            Expr *cast_expr;
        };
        struct {
            // Unary
            Expr *operand;
            union {
                struct {
                    size_t num_args;
                    Expr **args;
                };
                Expr *index;
                const char *field;
            };
        };
        struct {
            // Binary
            Expr *left;
            Expr *right;
        };
        struct {
            // Ternary
            Expr *cond;
            Expr *then_expr;
            Expr *else_expr;
        };
    };
};

typedef enum StmtKind {
    STMT_NONE,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_DO,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_AUTO_ASSIGN,
    STMT_EXPR,
} StmtKind;

typedef struct StmtBlock {
    size_t num_stmts;
    Stmt **stmts;
} StmtBlock;

typedef struct ElseIf {
    Expr *cond;
    StmtBlock block;
} ElseIf;

typedef struct Case {
    size_t num_exprs;
    Expr **exprs;
    StmtBlock block;
} Case;

struct Stmt {
    StmtKind kind;
    Expr *expr;
    StmtBlock block;
    union {
        // If
        struct {
            size_t num_elseifs;
            ElseIf *elseifs;
            StmtBlock else_block;
        };
        // For
        struct {
            StmtBlock for_init;
            StmtBlock for_next;
        };
        // Switch
        struct {
            size_t num_cases;
            Case *cases;
        };
        // Auto-assign
        struct {
            const char *name;
        };
        // Assignment operators
        struct {
            Expr *rhs;
        };
    };
};
