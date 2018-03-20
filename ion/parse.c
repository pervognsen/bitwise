Decl *parse_decl();
Typespec *parse_type();
Stmt *parse_stmt();
Expr *parse_expr();

Typespec *parse_type_func() {
    Typespec **args = NULL;
    expect_token('(');
    if (!is_token(')')) {
        buf_push(args, parse_type());
        while (match_token(',')) {
            buf_push(args, parse_type());
        }
    }
    expect_token(')');
    Typespec *ret = NULL;
    if (match_token(':')) {
        ret = parse_type();
    }
    return typespec_func(ast_dup(args, buf_sizeof(args)), buf_len(args), ret);
}

Typespec *parse_type_base() {
    if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        return typespec_name(name);
    } else if (match_keyword(func_keyword)) {
        return parse_type_func();
    } else if (match_token('(')) {
        return parse_type();
    } else {
        fatal_syntax_error("Unexpected token %s in type", temp_token_kind_str(token.kind));
        return NULL;
    }
}

Typespec *parse_type() {
    Typespec *type = parse_type_base();
    while (is_token('[') || is_token('*')) {
        if (match_token('[')) {
            Expr *expr = NULL;
            if (!is_token(']')) {
                expr = parse_expr();
            }
            expect_token(']');
            type = typespec_array(type, expr);
        } else {
            assert(is_token('*'));
            next_token();
            type = typespec_ptr(type);
        }
    }
    return type;
}

Expr *parse_expr_compound(Typespec *type) {
    expect_token('{');
    Expr **args = NULL;
    if (!is_token('}')) {
        buf_push(args, parse_expr());
        while (match_token(',')) {
            buf_push(args, parse_expr());
        }
    }
    expect_token('}');
    return expr_compound(type, ast_dup(args, buf_sizeof(args)), buf_len(args));
}

Expr *parse_expr_operand() {
    if (is_token(TOKEN_INT)) {
        uint64_t val = token.int_val;
        next_token();
        return expr_int(val);
    } else if (is_token(TOKEN_FLOAT)) {
        double val = token.float_val;
        next_token();
        return expr_float(val);
    } else if (is_token(TOKEN_STR)) {
        const char *val = token.str_val;
        next_token();
        return expr_str(val);
    } else if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        if (is_token('{')) {
            return parse_expr_compound(typespec_name(name));
        } else {
            return expr_name(name);
        }
    } else if (is_token('{')) {
        return parse_expr_compound(NULL);
    } else if (match_token('(')) {
        if (is_token(':')) {
            Typespec *type = parse_type();
            expect_token(')');
            return parse_expr_compound(type);
        } else {
            Expr *expr = parse_expr();
            expect_token(')');
            return expr;
        }
    } else {
        fatal_syntax_error("Unexpected token %s in expression", temp_token_kind_str(token.kind));
        return NULL;
    }
}

Expr *parse_expr_base() {
    Expr *expr = parse_expr_operand();
    while (is_token('(') || is_token('[') || is_token('.')) {
        if (match_token('(')) {
            Expr **args = NULL;
            if (!is_token(')')) {
                buf_push(args, parse_expr());
                while (match_token(',')) {
                    buf_push(args, parse_expr());
                }
            }
            expect_token(')');
            expr = expr_call(expr, ast_dup(args, buf_sizeof(args)), buf_len(args));
        } else if (match_token('[')) {
            Expr *index = parse_expr();
            expect_token(']');
            expr = expr_index(expr, index);
        } else {
            assert(is_token('.'));
            next_token();
            const char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = expr_field(expr, field);
        }
    }
    return expr;
}

bool is_unary_op() {
    return is_token('+') || is_token('-') || is_token('*') || is_token('&');
}

Expr *parse_expr_unary() {
    if (is_unary_op()) {
        TokenKind op = token.kind;
        next_token();
        return expr_unary(op, parse_expr_unary());
    } else {
        return parse_expr_base();
    }
}

bool is_mul_op() {
    return is_token('*') || is_token('/') || is_token('%') || is_token('&') || is_token(TOKEN_LSHIFT) || is_token(TOKEN_RSHIFT);
}

Expr *parse_expr_mul() {
    Expr *expr = parse_expr_unary();
    while (is_mul_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_unary());
    }
    return expr;
}

bool is_add_op() {
    return is_token('+') || is_token('-') || is_token('|') || is_token('^');
}

Expr *parse_expr_add() {
    Expr *expr = parse_expr_mul();
    while (is_add_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_mul());
    }
    return expr;
}

bool is_cmp_op() {
    return is_token('<') || is_token('>') || is_token(TOKEN_EQ) || is_token(TOKEN_NOTEQ) || is_token(TOKEN_GTEQ) || is_token(TOKEN_LTEQ);
}

Expr *parse_expr_cmp() {
    Expr *expr = parse_expr_add();
    while (is_cmp_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_add());
    }
    return expr;
}

Expr *parse_expr_and() {
    Expr *expr = parse_expr_cmp();
    while (match_token(TOKEN_AND)) {
        expr = expr_binary(TOKEN_AND, expr, parse_expr_cmp());
    }
    return expr;
}

Expr *parse_expr_or() {
    Expr *expr = parse_expr_and();
    while (match_token(TOKEN_OR)) {
        expr = expr_binary(TOKEN_OR, expr, parse_expr_and());
    }
    return expr;
}

Expr *parse_expr_ternary() {
    Expr *expr = parse_expr_or();
    if (match_token('?')) {
        Expr *then_expr = parse_expr_ternary();
        expect_token(':');
        Expr *else_expr = parse_expr_ternary();
        expr = expr_ternary(expr, then_expr, else_expr);
    }
    return expr;
}

Expr *parse_expr() {
    return parse_expr_ternary();
}

Expr *parse_paren_expr() {
    expect_token('(');
    Expr *expr = parse_expr();
    expect_token(')');
    return expr;
}

StmtBlock parse_stmt_block() {
    expect_token('{');
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token('}')) {
        buf_push(stmts, parse_stmt());
    }
    expect_token('}');
    return (StmtBlock){ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts)};
}

Stmt *parse_stmt_if() {
    Expr *cond = parse_paren_expr();
    StmtBlock then_block = parse_stmt_block();
    StmtBlock else_block = {0};
    ElseIf *elseifs = NULL;
    while (match_keyword(else_keyword)) {
        if (!match_keyword(if_keyword)) {
            else_block = parse_stmt_block();
            break;
        }
        Expr *elseif_cond = parse_paren_expr();
        StmtBlock elseif_block = parse_stmt_block();
        buf_push(elseifs, (ElseIf){elseif_cond, elseif_block});
    }
    return stmt_if(cond, then_block, ast_dup(elseifs, buf_sizeof(elseifs)), buf_len(elseifs), else_block);
}

Stmt *parse_stmt_while() {
    Expr *cond = parse_paren_expr();
    return stmt_while(cond, parse_stmt_block());
}

Stmt *parse_stmt_do_while() {
    StmtBlock block = parse_stmt_block();
    if (!match_keyword(while_keyword)) {
        fatal_syntax_error("Expected 'while' after 'do' block");
        return NULL;
    }
    Expr *cond = parse_paren_expr();
    Stmt *stmt = stmt_do_while(parse_paren_expr(), block);
    expect_token(';');
    return stmt;
}

Stmt *parse_stmt_for() {
    expect_token('(');
    Stmt *init = NULL;
    if (!is_token(';')) {
        init = parse_stmt();
    }
    Expr *cond = NULL;
    if (!is_token(';')) {
        cond = parse_expr();
    }
    expect_token(';');
    Stmt *next = NULL;
    if (!is_token(')')) {
        next = parse_stmt();
    }
    expect_token(')');
    return stmt_for(init, cond, next, parse_stmt_block());
}

SwitchCase parse_stmt_switch_case() {
    Expr **exprs = NULL;
    bool is_default = false;
    while (is_keyword(case_keyword) || is_keyword(default_keyword)) {
        if (match_keyword(case_keyword)) {
            buf_push(exprs, parse_expr());
            expect_token(':');
        } else {
            assert(is_keyword(default_keyword));
            next_token();
            is_default = true;
        }
    }
    StmtBlock block = parse_stmt_block();
    return (SwitchCase){ast_dup(exprs, buf_sizeof(exprs)), buf_len(exprs), is_default, block};
}

Stmt *parse_stmt_switch() {
    Expr *expr = parse_paren_expr();
    SwitchCase *cases = NULL;
    expect_token('{');
    while (!is_token_eof() && !is_token('}')) {
        buf_push(cases, parse_stmt_switch_case());
    }
    expect_token('}');
    return stmt_switch(expr, ast_dup(cases, buf_sizeof(cases)), buf_len(cases));
}

bool is_assign_op() {
    return TOKEN_FIRST_ASSIGN <= token.kind && token.kind <= TOKEN_LAST_ASSIGN;
}

Stmt *parse_stmt() {
    if (is_token('{')) {
        return stmt_block(parse_stmt_block());
    } else if (match_keyword(return_keyword)) {
        Stmt *stmt = stmt_return(parse_expr());
        expect_token(';');
        return stmt;
    } else if (match_keyword(break_keyword)) {
        expect_token(';');
        return stmt_break();
    } else if (match_keyword(continue_keyword)) {
        expect_token(';');
        return stmt_continue();
    } else if (match_keyword(if_keyword)) {
        return parse_stmt_if();
    } else if (match_keyword(while_keyword)) {
        return parse_stmt_while();
    } else if (match_keyword(do_keyword)) {
        return parse_stmt_do_while();
    } else if (match_keyword(for_keyword)) {
        return parse_stmt_for();
    } else if (match_keyword(switch_keyword)) {
        return parse_stmt_switch();
    } else {
        Expr *expr = parse_expr();
        Stmt *stmt;
        if (match_token(TOKEN_COLON_ASSIGN)) {
            if (expr->kind != EXPR_NAME) {
                fatal_syntax_error(":= must be preceded by a name");
            }
            stmt = stmt_init(expr->name, parse_expr());
        } else if (is_assign_op()) {
            TokenKind op = token.kind;
            next_token();
            stmt = stmt_assign(op, expr, parse_expr());
        } else {
            stmt = stmt_expr(expr);
        }
        expect_token(';');
        return stmt;
    }
}

const char *parse_name() {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

Decl *parse_decl_enum() {
    const char *name = parse_name();
    expect_token('{');
    EnumItem *items = NULL;
    while (!is_token_eof() && !is_token('}')) {
        const char *item_name = token.name;
        expect_token(TOKEN_NAME);
        Expr *expr = NULL;
        if (match_token('=')) {
            expr = parse_expr();
        }
        buf_push(items, (EnumItem){name, expr});
    }
    expect_token('}');
    return decl_enum(name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

AggregateItem parse_decl_aggregate_item() {
    const char **names = NULL;
    buf_push(names, parse_name());
    while (match_token(',')) {
        buf_push(names, parse_name());
    }
    expect_token(':');
    Typespec *type = parse_type();
    expect_token(';');
    return (AggregateItem){ast_dup(names, buf_sizeof(names)), buf_len(names), type};
}

Decl *parse_decl_aggregate(DeclKind kind) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    const char *name = parse_name();
    expect_token('{');
    AggregateItem *items = NULL;
    while (!is_token_eof() && !is_token('}')) {
        buf_push(items, parse_decl_aggregate_item());
    }
    expect_token('}');
    return decl_aggregate(kind, name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

Decl *parse_decl_var() {
    const char *name = parse_name();
    if (match_token('=')) {
        return decl_var(name, NULL, parse_expr());
    } else if (match_token(':')) {
        Typespec *type = parse_type();
        Expr *expr = NULL;
        if (match_token('=')) {
            expr = parse_expr();
        }
        return decl_var(name, type, expr);
    } else {
        fatal_syntax_error("Expected : or = after var, got %s", temp_token_kind_str(token.kind));
        return NULL;
    }
}

Decl *parse_decl_const() {
    const char *name = parse_name();
    expect_token('=');
    return decl_const(name, parse_expr());
}

Decl *parse_decl_typedef() {
    const char *name = parse_name();
    expect_token('=');
    return decl_typedef(name, parse_type());
}

FuncParam parse_decl_func_param() {
    const char *name = parse_name();
    expect_token(':');
    Typespec *type = parse_type();
    return (FuncParam){name, type};
}

Decl *parse_decl_func() {
    const char *name = parse_name();
    expect_token('(');
    FuncParam *params = NULL;
    if (!is_token(')')) {
        buf_push(params, parse_decl_func_param());
        while (match_token(',')) {
            buf_push(params, parse_decl_func_param());
        }
    }
    expect_token(')');
    Typespec *ret_type = NULL;
    if (match_token(':')) {
        ret_type = parse_type();
    }
    StmtBlock block = parse_stmt_block();
    return decl_func(name, ast_dup(params, buf_sizeof(params)), buf_len(params), ret_type, block);
}

Decl *parse_decl() {
    if (match_keyword(enum_keyword)) {
        return parse_decl_enum();
    } else if (match_keyword(struct_keyword)) {
        return parse_decl_aggregate(DECL_STRUCT);
    } else if (match_keyword(union_keyword)) {
        return parse_decl_aggregate(DECL_UNION);
    } else if (match_keyword(var_keyword)) {
        return parse_decl_var();
    } else if (match_keyword(const_keyword)) {
        return parse_decl_const();
    } else if (match_keyword(typedef_keyword)) {
        return parse_decl_typedef();
    } else if (match_keyword(func_keyword)) {
        return parse_decl_func();
    } else {
        fatal_syntax_error("Expected declaration keyword, got %s", temp_token_kind_str(token.kind));
        return NULL;
    }
}

void parse_and_print_decl(const char *str) {
    init_stream(str);
    Decl *decl = parse_decl();
    print_decl(decl);
    printf("\n");
}

void parse_test() {
    parse_and_print_decl("func fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }");
    parse_and_print_decl("var x = b == 1 ? 1+2 : 3-4");
    parse_and_print_decl("const pi = 3.14");
    parse_and_print_decl("struct Vector { x, y: float; }");
    parse_and_print_decl("union IntOrFloat { i: int; f: float; }");
    parse_and_print_decl("typedef Vectors = Vector[1+2]");
}
