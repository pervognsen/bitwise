#include "ast.h"

Typespec *typespec_alloc(TypespecKind kind) {
    Typespec *t = xcalloc(1, sizeof(Typespec));
    t->kind = kind;
    return t;
}

Typespec *typespec_name(const char *name) {
    Typespec *t = typespec_alloc(TYPESPEC_NAME);
    t->name = name;
    return t;
}

Typespec *typespec_pointer(Typespec *base) {
    Typespec *t = typespec_alloc(TYPESPEC_POINTER);
    t->base = base;
    return t;
}

Typespec *typespec_array(Typespec *base, Expr *size) {
    Typespec *t = typespec_alloc(TYPESPEC_ARRAY);
    t->base = base;
    t->size = size;
    return t;
}

Typespec *typespec_func(FuncTypespec func) {
    Typespec *t = typespec_alloc(TYPESPEC_FUNC);
    t->func = func;
    return t;
}

Expr *expr_alloc(ExprKind kind) {
    Expr *e = xcalloc(1, sizeof(Expr));
    e->kind = kind;
    return e;
}

Expr *expr_int(uint64_t int_val) {
    Expr *e = expr_alloc(EXPR_INT);
    e->int_val = int_val;
    return e;
}

Expr *expr_float(double float_val) {
    Expr *e = expr_alloc(EXPR_FLOAT);
    e->float_val = float_val;
    return e;
}

Expr *expr_str(const char *str_val) {
    Expr *e = expr_alloc(EXPR_STR);
    e->str_val = str_val;
    return e;
}

Expr *expr_name(const char *name) {
    Expr *e = expr_alloc(EXPR_NAME);
    e->name = name;
    return e;
}

Expr *expr_cast(Typespec *type, Expr *expr) {
    Expr *e = expr_alloc(EXPR_CAST);
    e->cast_expr.type = type;
    e->cast_expr.expr = expr;
    return e;
}

Expr *expr_call(Expr *expr, Expr **args, size_t num_args) {
    Expr *e = expr_alloc(EXPR_CALL);
    e->call_expr.expr = expr;
    e->call_expr.args = args;
    e->call_expr.num_args = num_args;
    return e;
}

Expr *expr_index(Expr *expr, Expr *index) {
    Expr *e = expr_alloc(EXPR_INDEX);
    e->index_expr.expr = expr;
    e->index_expr.index = index;
    return e;
}

Expr *expr_field(Expr *expr, const char *name) {
    Expr *e = expr_alloc(EXPR_FIELD);
    e->field_expr.expr = expr;
    e->field_expr.name = name;
    return e;
}

Expr *expr_unary(TokenKind op, Expr *expr) {
    Expr *e = expr_alloc(EXPR_UNARY);
    e->unary_expr.op = op;
    e->unary_expr.expr = expr;
    return e;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *e = expr_alloc(EXPR_BINARY);
    e->binary_expr.op = op;
    e->binary_expr.left = left;
    e->binary_expr.right = right;
    return e;
}

Expr *expr_ternary(Expr *cond, Expr *if_true, Expr *if_false) {
    Expr *e = expr_alloc(EXPR_TERNARY);
    e->ternary_expr.cond = cond;
    e->ternary_expr.if_true = if_true;
    e->ternary_expr.if_false = if_false;
    return e;
}

void print_expr(Expr *expr);

void print_type(Typespec *type) {
    switch (type->kind) {
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_FUNC: {
        FuncTypespec func = type->func;
        printf("(func (");
        for (Typespec **it = func.args; it != func.args + func.num_args; it++) {
            printf(" ");
            print_type(*it);
        }
        printf(") ");
        print_type(func.ret);
        printf(")");
        break;
    }
    case TYPESPEC_ARRAY:
        printf("(arr ");
        print_type(type->base);
        printf(" ");
        print_expr(type->size);
        printf(")");
        break;
    case TYPESPEC_POINTER:
        printf("(ptr ");
        print_type(type->base);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_expr(Expr *expr) {
    Expr *e = expr;
    switch (e->kind) {
    case EXPR_INT:
        printf("%" PRIu64, e->int_val);
        break;
    case EXPR_FLOAT:
        printf("%f", e->float_val);
        break;
    case EXPR_STR:
        printf("\"%s\"", e->str_val);
        break;
    case EXPR_NAME:
        printf("%s", e->name);
        break;
    case EXPR_CAST:
        printf("(cast ");
        print_type(e->cast_expr.type);
        printf(" ");
        print_expr(e->cast_expr.expr);
        printf(")");
        break;
    case EXPR_CALL:
        printf("(");
        print_expr(e->call_expr.expr);
        for (Expr **it = e->call_expr.args; it != e->call_expr.args + e->call_expr.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_INDEX:
        printf("(index ");
        print_expr(e->index_expr.expr);
        printf(" ");
        print_expr(e->index_expr.index);
        printf(")");
        break;
    case EXPR_FIELD:
        printf("(field ");
        print_expr(e->field_expr.expr);
        printf(" %s)", e->field_expr.name);
        break;
    case EXPR_COMPOUND:
        printf("(compound ...)");
        break;
    case EXPR_UNARY:
        printf("(%c ", e->unary_expr.op);
        print_expr(e->unary_expr.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(%c ", e->binary_expr.op);
        print_expr(e->binary_expr.left);
        printf(" ");
        print_expr(e->binary_expr.right);
        printf(")");
        break;
    case EXPR_TERNARY:
        printf("(if ");
        print_expr(e->ternary_expr.cond);
        printf(" ");
        print_expr(e->ternary_expr.if_true);
        printf(" ");
        print_expr(e->ternary_expr.if_false);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_expr_line(Expr *expr) {
    print_expr(expr);
    printf("\n");
}

void expr_test() {
    Expr *exprs[] = {
        expr_binary('+', expr_int(1), expr_int(2)),
        expr_unary('-', expr_float(3.14)),
        expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false")),
        expr_field(expr_name("person"), "name"),
        expr_call(expr_name("fact"), (Expr*[]){expr_int(42)}, 1),
        expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3)),
        expr_cast(typespec_pointer(typespec_name("int")), expr_name("void_ptr")),
    };
    for (Expr **it = exprs; it != exprs + sizeof(exprs)/sizeof(*exprs); it++) {
        print_expr_line(*it);
    }
}

void ast_test() {
    expr_test();
}
