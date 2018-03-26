Decl *parse_decl_opt();
Decl *parse_decl();
Typespec *parse_type();
Stmt *parse_stmt();
Expr *parse_expr();

Typespec *parse_type_func(void) {
    Typespec **args = NULL;
    expect_token(TOKEN_LPAREN);
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(args, parse_type());
        while (match_token(TOKEN_COMMA)) {
            buf_push(args, parse_type());
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret = NULL;
    if (match_token(TOKEN_COLON)) {
        ret = parse_type();
    }
    return typespec_func(args, buf_len(args), ret);
}

Typespec *parse_type_base(void) {
    if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        return typespec_name(name);
    } else if (match_keyword(func_keyword)) {
        return parse_type_func();
    } else if (match_token(TOKEN_LPAREN)) {
        Typespec *type = parse_type();
        expect_token(TOKEN_RPAREN);
        return type;
    } else {
        fatal_syntax_error("Unexpected token %s in type", token_info());
        return NULL;
    }
}

Typespec *parse_type(void) {
    Typespec *type = parse_type_base();
    while (is_token(TOKEN_LBRACKET) || is_token(TOKEN_MUL)) {
        if (match_token(TOKEN_LBRACKET)) {
            Expr *expr = NULL;
            if (!is_token(TOKEN_RBRACKET)) {
                expr = parse_expr();
            }
            expect_token(TOKEN_RBRACKET);
            type = typespec_array(type, expr);
        } else {
            assert(is_token(TOKEN_MUL));
            next_token();
            type = typespec_ptr(type);
        }
    }
    return type;
}

Expr *parse_expr_compound(Typespec *type) {
    expect_token(TOKEN_LBRACE);
    Expr **args = NULL;
    if (!is_token(TOKEN_RBRACE)) {
        buf_push(args, parse_expr());
        while (match_token(TOKEN_COMMA)) {
            buf_push(args, parse_expr());
        }
    }
    expect_token(TOKEN_RBRACE);
    return expr_compound(type, args, buf_len(args));
}

Expr *parse_expr_operand(void) {
    if (is_token(TOKEN_INT)) {
        int64_t val = token.int_val;
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
        if (is_token(TOKEN_LBRACE)) {
            return parse_expr_compound(typespec_name(name));
        } else {
            return expr_name(name);
        }
    } else if (match_keyword(sizeof_keyword)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return expr_sizeof_type(type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return expr_sizeof_expr(expr);
        }
    } else if (is_token(TOKEN_LBRACE)) {
        return parse_expr_compound(NULL);
    } else if (match_token(TOKEN_LPAREN)) {
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return parse_expr_compound(type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return expr;
        }
    } else {
        fatal_syntax_error("Unexpected token %s in expression", token_info());
        return NULL;
    }
}

Expr *parse_expr_base(void) {
    Expr *expr = parse_expr_operand();
    while (is_token(TOKEN_LPAREN) || is_token(TOKEN_LBRACKET) || is_token(TOKEN_DOT)) {
        if (match_token(TOKEN_LPAREN)) {
            Expr **args = NULL;
            if (!is_token(TOKEN_RPAREN)) {
                buf_push(args, parse_expr());
                while (match_token(TOKEN_COMMA)) {
                    buf_push(args, parse_expr());
                }
            }
            expect_token(TOKEN_RPAREN);
            expr = expr_call(expr, args, buf_len(args));
        } else if (match_token(TOKEN_LBRACKET)) {
            Expr *index = parse_expr();
            expect_token(TOKEN_RBRACKET);
            expr = expr_index(expr, index);
        } else {
            assert(is_token(TOKEN_DOT));
            next_token();
            const char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = expr_field(expr, field);
        }
    }
    return expr;
}

bool is_unary_op(void) {
    return is_token(TOKEN_ADD) || is_token(TOKEN_SUB) || is_token(TOKEN_MUL) || is_token(TOKEN_AND);
}

Expr *parse_expr_unary(void) {
    if (is_unary_op()) {
        TokenKind op = token.kind;
        next_token();
        return expr_unary(op, parse_expr_unary());
    } else {
        return parse_expr_base();
    }
}

bool is_mul_op(void) {
    return TOKEN_FIRST_MUL <= token.kind && token.kind <= TOKEN_LAST_MUL;
}

Expr *parse_expr_mul(void) {
    Expr *expr = parse_expr_unary();
    while (is_mul_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_unary());
    }
    return expr;
}

bool is_add_op(void) {
    return TOKEN_FIRST_ADD <= token.kind && token.kind <= TOKEN_LAST_ADD;
}

Expr *parse_expr_add(void) {
    Expr *expr = parse_expr_mul();
    while (is_add_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_mul());
    }
    return expr;
}

bool is_cmp_op(void) {
    return TOKEN_FIRST_CMP <= token.kind && token.kind <= TOKEN_LAST_CMP;
}

Expr *parse_expr_cmp(void) {
    Expr *expr = parse_expr_add();
    while (is_cmp_op()) {
        TokenKind op = token.kind;
        next_token();
        expr = expr_binary(op, expr, parse_expr_add());
    }
    return expr;
}

Expr *parse_expr_and(void) {
    Expr *expr = parse_expr_cmp();
    while (match_token(TOKEN_AND_AND)) {
        expr = expr_binary(TOKEN_AND_AND, expr, parse_expr_cmp());
    }
    return expr;
}

Expr *parse_expr_or(void) {
    Expr *expr = parse_expr_and();
    while (match_token(TOKEN_OR_OR)) {
        expr = expr_binary(TOKEN_OR_OR, expr, parse_expr_and());
    }
    return expr;
}

Expr *parse_expr_ternary(void) {
    Expr *expr = parse_expr_or();
    if (match_token(TOKEN_QUESTION)) {
        Expr *then_expr = parse_expr_ternary();
        expect_token(TOKEN_COLON);
        Expr *else_expr = parse_expr_ternary();
        expr = expr_ternary(expr, then_expr, else_expr);
    }
    return expr;
}

Expr *parse_expr(void) {
    return parse_expr_ternary();
}

Expr *parse_paren_expr(void) {
    expect_token(TOKEN_LPAREN);
    Expr *expr = parse_expr();
    expect_token(TOKEN_RPAREN);
    return expr;
}

StmtList parse_stmt_block(void) {
    expect_token(TOKEN_LBRACE);
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(stmts, parse_stmt());
    }
    expect_token(TOKEN_RBRACE);
    return stmt_list(stmts, buf_len(stmts));
}

Stmt *parse_stmt_if(void) {
    Expr *cond = parse_paren_expr();
    StmtList then_block = parse_stmt_block();
    StmtList else_block = {0};
    ElseIf *elseifs = NULL;
    while (match_keyword(else_keyword)) {
        if (!match_keyword(if_keyword)) {
            else_block = parse_stmt_block();
            break;
        }
        Expr *elseif_cond = parse_paren_expr();
        StmtList elseif_block = parse_stmt_block();
        buf_push(elseifs, (ElseIf){elseif_cond, elseif_block});
    }
    return stmt_if(cond, then_block, elseifs, buf_len(elseifs), else_block);
}

Stmt *parse_stmt_while(void) {
    Expr *cond = parse_paren_expr();
    return stmt_while(cond, parse_stmt_block());
}

Stmt *parse_stmt_do_while(void) {
    StmtList block = parse_stmt_block();
    if (!match_keyword(while_keyword)) {
        fatal_syntax_error("Expected 'while' after 'do' block");
        return NULL;
    }
    Stmt *stmt = stmt_do_while(parse_paren_expr(), block);
    expect_token(TOKEN_SEMICOLON);
    return stmt;
}

bool is_assign_op(void) {
    return TOKEN_FIRST_ASSIGN <= token.kind && token.kind <= TOKEN_LAST_ASSIGN;
}

Stmt *parse_simple_stmt(void) {
    Expr *expr = parse_expr();
    Stmt *stmt;
    if (match_token(TOKEN_COLON_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            fatal_syntax_error(":= must be preceded by a name");
            return NULL;
        }
        stmt = stmt_init(expr->name, parse_expr());
    } else if (is_assign_op()) {
        TokenKind op = token.kind;
        next_token();
        stmt = stmt_assign(op, expr, parse_expr());
    } else if (is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
        TokenKind op = token.kind;
        next_token();
        stmt = stmt_assign(op, expr, NULL);
    } else {
        stmt = stmt_expr(expr);
    }
    return stmt;
}

Stmt *parse_stmt_for(void) {
    expect_token(TOKEN_LPAREN);
    Stmt *init = NULL;
    if (!is_token(TOKEN_SEMICOLON)) {
        init = parse_simple_stmt();
    }
    expect_token(TOKEN_SEMICOLON);
    Expr *cond = NULL;
    if (!is_token(TOKEN_SEMICOLON)) {
        cond = parse_expr();
    }
    expect_token(TOKEN_SEMICOLON);
    Stmt *next = NULL;
    if (!is_token(TOKEN_RPAREN)) {
        next = parse_simple_stmt();
        if (next->kind == STMT_INIT) {
            syntax_error("Init statements not allowed in for-statement's next clause");
        }
    }
    expect_token(TOKEN_RPAREN);
    return stmt_for(init, cond, next, parse_stmt_block());
}

SwitchCase parse_stmt_switch_case(void) {
    Expr **exprs = NULL;
    bool is_default = false;
    while (is_keyword(case_keyword) || is_keyword(default_keyword)) {
        if (match_keyword(case_keyword)) {
            buf_push(exprs, parse_expr());
        } else {
            assert(is_keyword(default_keyword));
            next_token();
            if (is_default) {
                syntax_error("Duplicate default labels in same switch clause");
            }
            is_default = true;
        }
        expect_token(TOKEN_COLON);
    }
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE) && !is_keyword(case_keyword) && !is_keyword(default_keyword)) {
        buf_push(stmts, parse_stmt());
    }
    return (SwitchCase){exprs, buf_len(exprs), is_default, stmt_list(stmts, buf_len(stmts))};
}

Stmt *parse_stmt_switch(void) {
    Expr *expr = parse_paren_expr();
    SwitchCase *cases = NULL;
    expect_token(TOKEN_LBRACE);
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(cases, parse_stmt_switch_case());
    }
    expect_token(TOKEN_RBRACE);
    return stmt_switch(expr, cases, buf_len(cases));
}

Stmt *parse_stmt(void) {
    if (match_keyword(if_keyword)) {
        return parse_stmt_if();
    } else if (match_keyword(while_keyword)) {
        return parse_stmt_while();
    } else if (match_keyword(do_keyword)) {
        return parse_stmt_do_while();
    } else if (match_keyword(for_keyword)) {
        return parse_stmt_for();
    } else if (match_keyword(switch_keyword)) {
        return parse_stmt_switch();
    } else if (is_token(TOKEN_LBRACE)) {
        return stmt_block(parse_stmt_block());
    } else if (match_keyword(break_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        return stmt_break();
    } else if (match_keyword(continue_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        return stmt_continue();
    } else if (match_keyword(return_keyword)) {
        Expr *expr = NULL;
        if (!is_token(TOKEN_SEMICOLON)) {
            expr = parse_expr();
        }
        expect_token(TOKEN_SEMICOLON);
        return stmt_return(expr);
    } else {
        Decl *decl = parse_decl_opt();
        if (decl) {
            return stmt_decl(decl);
        }
        Stmt *stmt = parse_simple_stmt();
        expect_token(TOKEN_SEMICOLON);
        return stmt;
    }
}

const char *parse_name(void) {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

EnumItem parse_decl_enum_item(void) {
    const char *name = parse_name();
    Expr *init = NULL;
    if (match_token(TOKEN_ASSIGN)) {
        init = parse_expr();
    }
    return (EnumItem){name, init};
}

Decl *parse_decl_enum(void) {
    const char *name = parse_name();
    expect_token(TOKEN_LBRACE);
    EnumItem *items = NULL;
    if (!is_token(TOKEN_RBRACE)) {
        buf_push(items, parse_decl_enum_item());
        while (match_token(TOKEN_COMMA)) {
            buf_push(items, parse_decl_enum_item());
        }
    }
    expect_token(TOKEN_RBRACE);
    return decl_enum(name, items, buf_len(items));
}

AggregateItem parse_decl_aggregate_item(void) {
    const char **names = NULL;
    buf_push(names, parse_name());
    while (match_token(TOKEN_COMMA)) {
        buf_push(names, parse_name());
    }
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    expect_token(TOKEN_SEMICOLON);
    return (AggregateItem){names, buf_len(names), type};
}

Decl *parse_decl_aggregate(DeclKind kind) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    const char *name = parse_name();
    expect_token(TOKEN_LBRACE);
    AggregateItem *items = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(items, parse_decl_aggregate_item());
    }
    expect_token(TOKEN_RBRACE);
    return decl_aggregate(kind, name, items, buf_len(items));
}

Decl *parse_decl_var(void) {
    const char *name = parse_name();
    if (match_token(TOKEN_ASSIGN)) {
        return decl_var(name, NULL, parse_expr());
    } else if (match_token(TOKEN_COLON)) {
        Typespec *type = parse_type();
        Expr *expr = NULL;
        if (match_token(TOKEN_ASSIGN)) {
            expr = parse_expr();
        }
        return decl_var(name, type, expr);
    } else {
        fatal_syntax_error("Expected : or = after var, got %s", token_info());
        return NULL;
    }
}

Decl *parse_decl_const(void) {
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    return decl_const(name, parse_expr());
}

Decl *parse_decl_typedef(void) {
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    return decl_typedef(name, parse_type());
}

FuncParam parse_decl_func_param(void) {
    const char *name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    return (FuncParam){name, type};
}

Decl *parse_decl_func(void) {
    const char *name = parse_name();
    expect_token(TOKEN_LPAREN);
    FuncParam *params = NULL;
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(params, parse_decl_func_param());
        while (match_token(TOKEN_COMMA)) {
            buf_push(params, parse_decl_func_param());
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret_type = NULL;
    if (match_token(TOKEN_COLON)) {
        ret_type = parse_type();
    }
    StmtList block = parse_stmt_block();
    return decl_func(name, params, buf_len(params), ret_type, block);
}

Decl *parse_decl_opt(void) {
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
        return NULL;
    }
}

Decl *parse_decl(void) {
    Decl *decl = parse_decl_opt();
    if (!decl) {
        fatal_syntax_error("Expected declaration keyword, got %s", token_info());
    }
    return decl;
}

void parse_test(void) {
    const char *decls[] = {
        "const n = sizeof(:int*[16])",
        "const n = sizeof(1+2)",
        "var x = b == 1 ? 1+2 : 3-4",
        "func fact(n: int): int { trace(\"fact\"); if (n == 0) { return 1; } else { return n * fact(n-1); } }",
        "func fact(n: int): int { p := 1; for (i := 1; i <= n; i++) { p *= i; } return p; }",
        "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0",
        "func f(x: int): bool { switch (x) { case 0: case 1: return true; case 2: default: return false; } }",
        "enum Color { RED = 3, GREEN, BLUE = 0 }",
        "const pi = 3.14",
        "struct Vector { x, y: float; }",
        "var v = Vector{1.0, -1.0}",
        "var v: Vector = {1.0, -1.0}",
        "union IntOrFloat { i: int; f: float; }",
        "typedef Vectors = Vector[1+2]",
        "func f() { do { print(42); } while(1); }",
        "typedef T = (func(int):int)[16]",
        "func f() { enum E { A, B, C } return; }",
        "func f() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }",
    };
    for (const char **it = decls; it != decls + sizeof(decls)/sizeof(*decls); it++) {
        init_stream(*it);
        Decl *decl = parse_decl();
        print_decl(decl);
        printf("\n");
    }
}
