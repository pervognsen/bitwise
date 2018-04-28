typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

typedef struct NoteArg {
    SrcPos pos;
    const char *name;
    Expr *expr;
} NoteArg;

typedef struct Note {
    SrcPos pos;
    const char *name;
    NoteArg *args;
    size_t num_args;
} Note;

typedef struct Notes {
    Note *notes;
    size_t num_notes;
} Notes;

typedef struct StmtList {
    SrcPos pos;
    Stmt **stmts;
    size_t num_stmts;
} StmtList;

typedef enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR,
    TYPESPEC_CONST,
} TypespecKind;

struct Typespec {
    TypespecKind kind;
    SrcPos pos;
    Typespec *base;
    union {
        const char *name;
        struct {
            Typespec **args;
            size_t num_args;
            bool has_varargs;
            Typespec *ret;
        } func;
        Expr *num_elems;
    };
};

typedef struct FuncParam {
    SrcPos pos;
    const char *name;
    Typespec *type;
} FuncParam;

typedef struct AggregateItem {
    SrcPos pos;
    const char **names;
    size_t num_names;
    Typespec *type;
} AggregateItem;

typedef struct EnumItem {
    SrcPos pos;
    const char *name;
    Expr *init;
} EnumItem;

typedef struct ImportItem {
    const char *name;
    const char *rename;
} ImportItem;

typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPEDEF,
    DECL_FUNC,
    DECL_NOTE,
    DECL_IMPORT,
} DeclKind;

struct Decl {
    DeclKind kind;
    SrcPos pos;
    const char *name;
    Notes notes;
    bool is_incomplete;
    union {
        Note note;
        struct {
            Typespec *type;
            EnumItem *items;
            size_t num_items;
        } enum_decl;
        struct {
            AggregateItem *items;
            size_t num_items;
        } aggregate;
        struct {
            FuncParam *params;
            size_t num_params;
            Typespec *ret_type;
            bool has_varargs;
            StmtList block;
        } func;
        struct {
            Typespec *type;
        } typedef_decl;
        struct {
            Typespec *type;
            Expr *expr;
        } var;
        struct {
            Typespec *type;
            Expr *expr;
        } const_decl;
        struct {
            bool is_relative;
            const char **names;
            size_t num_names;
            bool import_all;
            ImportItem *items;
            size_t num_items;
        } import;
    };
};

typedef struct Decls {
    Decl **decls;
    size_t num_decls;
} Decls;

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_PAREN,
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
    EXPR_MODIFY,
    EXPR_SIZEOF_EXPR,
    EXPR_SIZEOF_TYPE,
    EXPR_TYPEOF_EXPR,
    EXPR_TYPEOF_TYPE,
    EXPR_ALIGNOF_EXPR,
    EXPR_ALIGNOF_TYPE,
    EXPR_OFFSETOF,
} ExprKind;

typedef enum CompoundFieldKind {
    FIELD_DEFAULT,
    FIELD_NAME,
    FIELD_INDEX,
} CompoundFieldKind;

typedef struct CompoundField {
    CompoundFieldKind kind;
    SrcPos pos;
    Expr *init;
    union {
        const char *name;
        Expr *index;
    };
} CompoundField;

struct Expr {
    ExprKind kind;
    SrcPos pos;
    union {
        struct {
            Expr *expr;
        } paren;
        struct {
            unsigned long long val;
            TokenMod mod;
            TokenSuffix suffix;
        } int_lit;
        struct {
            double val;
            TokenSuffix suffix;
        } float_lit;
        struct {
            const char *val;
            TokenMod mod;
        } str_lit;
        const char *name;
        Expr *sizeof_expr;
        Typespec *sizeof_type;
        Expr *typeof_expr;
        Typespec *typeof_type;
        Expr *alignof_expr;
        Typespec *alignof_type;
        struct {
            Typespec *type;
            const char *name;
        } offsetof_field;
        struct {
            Typespec *type;
            CompoundField *fields;
            size_t num_fields;
        } compound;
        struct {
            Typespec *type;
            Expr *expr;            
        } cast;
        struct {
            TokenKind op;
            bool post;
            Expr *expr;
        } modify;
        struct {
            TokenKind op;
            Expr *expr;
        } unary;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        } binary;
        struct {
            Expr *cond;
            Expr *then_expr;
            Expr *else_expr;
        } ternary;
        struct {
            Expr *expr;
            Expr **args;
            size_t num_args;            
        } call;
        struct {
            Expr *expr;
            Expr *index;
        } index;
        struct {
            Expr *expr;
            const char *name;
        } field;
    };
};

typedef struct ElseIf {
    Expr *cond;
    StmtList block;
} ElseIf;

typedef struct SwitchCase {
    Expr **exprs;
    size_t num_exprs;
    bool is_default;
    StmtList block;
} SwitchCase;

typedef enum StmtKind {
    STMT_NONE,
    STMT_DECL,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_DO_WHILE,
    STMT_FOR,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_INIT,
    STMT_EXPR,
    STMT_NOTE,
} StmtKind;

struct Stmt {
    StmtKind kind;
    Notes notes;
    SrcPos pos;
    union {
        Note note;
        Expr *expr;
        Decl *decl;
        struct {
            Stmt *init;
            Expr *cond;
            StmtList then_block;
            ElseIf *elseifs;
            size_t num_elseifs;
            StmtList else_block;            
        } if_stmt;
        struct {
            Expr *cond;
            StmtList block;
        } while_stmt;
        struct {
            Stmt *init;
            Expr *cond;
            Stmt *next;
            StmtList block;
        } for_stmt;
        struct {
            Expr *expr;
            SwitchCase *cases;
            size_t num_cases;            
        } switch_stmt;
        StmtList block;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        } assign;
        struct {
            const char *name;
            Typespec *type;
            Expr *expr;
        } init;
    };
};
