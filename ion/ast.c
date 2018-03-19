#include "ast.h"

Typespec *typespec_alloc(TypespecKind kind) {
    Typespec *type = xcalloc(1, sizeof(Typespec));
    type->kind = kind;
    return type;
}

Typespec *typespec_name(const char *name) {
    Typespec *type = typespec_alloc(TYPESPEC_NAME);
    type->name = name;
    return type;
}

Typespec *typespec_pointer(Typespec *base) {
    Typespec *type = typespec_alloc(TYPESPEC_POINTER);
    type->base = base;
    return type;
}

Typespec *typespec_array(Typespec *base, Expr *size) {
    Typespec *type = typespec_alloc(TYPESPEC_ARRAY);
    type->base = base;
    type->size = size;
    return type;
}

Typespec *typespec_func(FuncTypespec func) {
    Typespec *type = typespec_alloc(TYPESPEC_FUNC);
    type->func = func;
    return type;
}

Expr *expr_alloc(ExprKind kind) {
    Expr *expr = xcalloc(1, sizeof(Expr));
    expr->kind = kind;
    return expr;
}

Expr *expr_int(uint64_t int_val) {
    Expr *expr = expr_alloc(EXPR_INT);
    expr->int_val = int_val;
    return expr;
}

Expr *expr_float(double float_val) {
    Expr *expr = expr_alloc(EXPR_FLOAT);
    expr->float_val = float_val;
    return expr;
}

Expr *expr_str(const char *str_val) {
    Expr *expr = expr_alloc(EXPR_STR);
    expr->str_val = str_val;
    return expr;
}

Expr *expr_name(const char *name) {
    Expr *expr = expr_alloc(EXPR_NAME);
    expr->name = name;
    return expr;
}

Expr *expr_cast(Typespec *type, Expr *expr) {
    Expr *new_expr = expr_alloc(EXPR_CAST);
    new_expr->cast.type = type;
    new_expr->cast.expr = expr;
    return new_expr;
}

Expr *expr_call(Expr *expr, Expr **args, size_t num_args) {
    Expr *new_expr = expr_alloc(EXPR_CALL);
    new_expr->call.expr = expr;
    new_expr->call.args = args;
    new_expr->call.num_args = num_args;
    return new_expr;
}

Expr *expr_index(Expr *expr, Expr *index) {
    Expr *new_expr = expr_alloc(EXPR_INDEX);
    new_expr->index.expr = expr;
    new_expr->index.index = index;
    return new_expr;
}

Expr *expr_field(Expr *expr, const char *name) {
    Expr *new_expr = expr_alloc(EXPR_FIELD);
    new_expr->field.expr = expr;
    new_expr->field.name = name;
    return new_expr;
}

Expr *expr_unary(TokenKind op, Expr *expr) {
    Expr *new_expr = expr_alloc(EXPR_UNARY);
    new_expr->unary.op = op;
    new_expr->unary.expr = expr;
    return new_expr;
}

Expr *expr_binary(TokenKind op, Expr *left, Expr *right) {
    Expr *new_expr = expr_alloc(EXPR_BINARY);
    new_expr->binary.op = op;
    new_expr->binary.left = left;
    new_expr->binary.right = right;
    return new_expr;
}

Expr *expr_ternary(Expr *cond, Expr *if_true, Expr *if_false) {
    Expr *new_expr = expr_alloc(EXPR_TERNARY);
    new_expr->ternary.cond = cond;
    new_expr->ternary.if_true = if_true;
    new_expr->ternary.if_false = if_false;
    return new_expr;
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
    switch (expr->kind) {
    case EXPR_INT:
        printf("%" PRIu64, expr->int_val);
        break;
    case EXPR_FLOAT:
        printf("%f", expr->float_val);
        break;
    case EXPR_STR:
        printf("\"%s\"", expr->str_val);
        break;
    case EXPR_NAME:
        printf("%s", expr->name);
        break;
    case EXPR_CAST:
        printf("(cast ");
        print_type(expr->cast.type);
        printf(" ");
        print_expr(expr->cast.expr);
        printf(")");
        break;
    case EXPR_CALL:
        printf("(");
        print_expr(expr->call.expr);
        for (Expr **it = expr->call.args; it != expr->call.args + expr->call.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_INDEX:
        printf("(index ");
        print_expr(expr->index.expr);
        printf(" ");
        print_expr(expr->index.index);
        printf(")");
        break;
    case EXPR_FIELD:
        printf("(field ");
        print_expr(expr->field.expr);
        printf(" %s)", expr->field.name);
        break;
    case EXPR_COMPOUND:
        printf("(compound ...)");
        break;
    case EXPR_UNARY:
        printf("(%c ", expr->unary.op);
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(%c ", expr->binary.op);
        print_expr(expr->binary.left);
        printf(" ");
        print_expr(expr->binary.right);
        printf(")");
        break;
    case EXPR_TERNARY:
        printf("(if ");
        print_expr(expr->ternary.cond);
        printf(" ");
        print_expr(expr->ternary.if_true);
        printf(" ");
        print_expr(expr->ternary.if_false);
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
