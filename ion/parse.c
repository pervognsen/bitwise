Decl *parse_decl_opt(void);
Decl *parse_decl(void);
Typespec *parse_type(void);
Stmt *parse_stmt(void);
Expr *parse_expr(void);
const char *parse_name(void);
Notes parse_notes(void);

Typespec *parse_type_func_param(void) {
    Typespec *type = parse_type();
    if (match_token(TOKEN_COLON)) {
        if (type->kind != TYPESPEC_NAME) {
            error_here("Colons in parameters of func types must be preceded by names.");
        }
        type = parse_type();
    }
    return type;
}

Typespec *parse_type_func(void) {
    SrcPos pos = token.pos;
    Typespec **args = NULL;
    bool has_varargs = false;
    expect_token(TOKEN_LPAREN);
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(args, parse_type_func_param());
        while (match_token(TOKEN_COMMA)) {
            if (match_token(TOKEN_ELLIPSIS)) {
                if (has_varargs) {
                    error_here("Multiple ellipsis instances in function type");
                }
                has_varargs = true;
            } else {
                if (has_varargs) {
                    error_here("Ellipsis must be last parameter in function type");
                }
                buf_push(args, parse_type_func_param());
            }
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret = NULL;
    if (match_token(TOKEN_COLON)) {
        ret = parse_type();
    }
    return new_typespec_func(pos, args, buf_len(args), ret, has_varargs);
}

Typespec *parse_type_base(void) {
    if (is_token(TOKEN_NAME)) {
        SrcPos pos = token.pos;
        const char *name = token.name;
        next_token();
        return new_typespec_name(pos, name);
    } else if (match_keyword(func_keyword)) {
        return parse_type_func();
    } else if (match_token(TOKEN_LPAREN)) {
        Typespec *type = parse_type();
        expect_token(TOKEN_RPAREN);
        return type;
    } else {
        fatal_error_here("Unexpected token %s in type", token_info());
        return NULL;
    }
}

Typespec *parse_type(void) {
    Typespec *type = parse_type_base();
    SrcPos pos = token.pos;
    while (is_token(TOKEN_LBRACKET) || is_token(TOKEN_MUL) || is_keyword(const_keyword)) {
        if (match_token(TOKEN_LBRACKET)) {
            Expr *size = NULL;
            if (!is_token(TOKEN_RBRACKET)) {
                size = parse_expr();
            }
            expect_token(TOKEN_RBRACKET);
            type = new_typespec_array(pos, type, size);
        } else if (match_keyword(const_keyword)) {
            type = new_typespec_const(pos, type);
        } else {
            assert(is_token(TOKEN_MUL));
            next_token();
            type = new_typespec_ptr(pos, type);
        }
    }
    return type;
}

CompoundField parse_expr_compound_field(void) {
    SrcPos pos = token.pos;
    if (match_token(TOKEN_LBRACKET)) {
        Expr *index = parse_expr();
        expect_token(TOKEN_RBRACKET);
        expect_token(TOKEN_ASSIGN);
        return (CompoundField){FIELD_INDEX, pos, parse_expr(), .index = index};
    } else {
        Expr *expr = parse_expr();
        if (match_token(TOKEN_ASSIGN)) {
            if (expr->kind != EXPR_NAME) {
                fatal_error_here("Named initializer in compound literal must be preceded by field name");
            }
            return (CompoundField){FIELD_NAME, pos, parse_expr(), .name = expr->name};
        } else {
            return (CompoundField){FIELD_DEFAULT, pos, expr};
        }
    }
}

Expr *parse_expr_compound(Typespec *type) {
    SrcPos pos = token.pos;
    expect_token(TOKEN_LBRACE);
    CompoundField *fields = NULL;
    while (!is_token(TOKEN_RBRACE)) {
        buf_push(fields, parse_expr_compound_field());
        if (!match_token(TOKEN_COMMA)) {
            break;
        }
    }
    expect_token(TOKEN_RBRACE);
    return new_expr_compound(pos, type, fields, buf_len(fields));
}

Expr *parse_expr_unary(void);

Expr *parse_expr_operand(void) {
    SrcPos pos = token.pos;
    if (is_token(TOKEN_INT)) {
        unsigned long long val = token.int_val;
        TokenMod mod = token.mod;
        TokenSuffix suffix = token.suffix;
        next_token();
        return new_expr_int(pos, val, mod, suffix);
    } else if (is_token(TOKEN_FLOAT)) {
        double val = token.float_val;
        TokenSuffix suffix = token.suffix;
        next_token();
        return new_expr_float(pos, val, suffix);
    } else if (is_token(TOKEN_STR)) {
        const char *val = token.str_val;
        TokenMod mod = token.mod;
        next_token();
        return new_expr_str(pos, val, mod);
    } else if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        next_token();
        if (is_token(TOKEN_LBRACE)) {
            return parse_expr_compound(new_typespec_name(pos, name));
        } else {
            return new_expr_name(pos, name);
        }
    } else if (match_keyword(sizeof_keyword)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return new_expr_sizeof_type(pos, type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return new_expr_sizeof_expr(pos, expr);
        }
    } else if (match_keyword(alignof_keyword)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return new_expr_alignof_type(pos, type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return new_expr_alignof_expr(pos, expr);
        }
    } else if (match_keyword(typeof_keyword)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return new_expr_typeof_type(pos, type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return new_expr_typeof_expr(pos, expr);
        }
    } else if (match_keyword(offsetof_keyword)) {
        expect_token(TOKEN_LPAREN);
        Typespec *type = parse_type();
        expect_token(TOKEN_COMMA);
        const char *name = parse_name();
        expect_token(TOKEN_RPAREN);
        return new_expr_offsetof(pos, type, name);
    } else if (is_token(TOKEN_LBRACE)) {
        return parse_expr_compound(NULL);
    } else if (match_token(TOKEN_LPAREN)) {
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            if (is_token(TOKEN_LBRACE)) {
                return parse_expr_compound(type);
            } else {
                return new_expr_cast(pos, type, parse_expr_unary());
            }
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return new_expr_paren(pos, expr);
        }
    } else {
        fatal_error_here("Unexpected token %s in expression", token_info());
        return NULL;
    }
}

Expr *parse_expr_base(void) {
    Expr *expr = parse_expr_operand();
    while (is_token(TOKEN_LPAREN) || is_token(TOKEN_LBRACKET) || is_token(TOKEN_DOT) || is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
        SrcPos pos = token.pos;
        if (match_token(TOKEN_LPAREN)) {
            Expr **args = NULL;
            if (!is_token(TOKEN_RPAREN)) {
                buf_push(args, parse_expr());
                while (match_token(TOKEN_COMMA)) {
                    buf_push(args, parse_expr());
                }
            }
            expect_token(TOKEN_RPAREN);
            expr = new_expr_call(pos, expr, args, buf_len(args));
        } else if (match_token(TOKEN_LBRACKET)) {
            Expr *index = parse_expr();
            expect_token(TOKEN_RBRACKET);
            expr = new_expr_index(pos, expr, index);
        } else if (is_token(TOKEN_DOT)) {
            next_token();
            const char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = new_expr_field(pos, expr, field);
        } else {
            assert(is_token(TOKEN_INC) || is_token(TOKEN_DEC));
            TokenKind op = token.kind;
            next_token();
            expr = new_expr_modify(pos, op, true, expr);
        }
    }
    return expr;
}

bool is_unary_op(void) {
    return
        is_token(TOKEN_ADD) ||
        is_token(TOKEN_SUB) ||
        is_token(TOKEN_MUL) ||
        is_token(TOKEN_AND) ||
        is_token(TOKEN_NEG) ||
        is_token(TOKEN_NOT) ||
        is_token(TOKEN_INC) ||
        is_token(TOKEN_DEC);
}

Expr *parse_expr_unary(void) {
    if (is_unary_op()) {
        SrcPos pos = token.pos;
        TokenKind op = token.kind;
        next_token();
        if (op == TOKEN_INC || op == TOKEN_DEC) {
            return new_expr_modify(pos, op, false, parse_expr_unary());
        } else {
            return new_expr_unary(pos, op, parse_expr_unary());
        }
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
        SrcPos pos = token.pos;
        TokenKind op = token.kind;
        next_token();
        expr = new_expr_binary(pos, op, expr, parse_expr_unary());
    }
    return expr;
}

bool is_add_op(void) {
    return TOKEN_FIRST_ADD <= token.kind && token.kind <= TOKEN_LAST_ADD;
}

Expr *parse_expr_add(void) {
    Expr *expr = parse_expr_mul();
    while (is_add_op()) {
        SrcPos pos = token.pos;
        TokenKind op = token.kind;
        next_token();
        expr = new_expr_binary(pos, op, expr, parse_expr_mul());
    }
    return expr;
}

bool is_cmp_op(void) {
    return TOKEN_FIRST_CMP <= token.kind && token.kind <= TOKEN_LAST_CMP;
}

Expr *parse_expr_cmp(void) {
    Expr *expr = parse_expr_add();
    while (is_cmp_op()) {
        SrcPos pos = token.pos;
        TokenKind op = token.kind;
        next_token();
        expr = new_expr_binary(pos, op, expr, parse_expr_add());
    }
    return expr;
}

Expr *parse_expr_and(void) {
    Expr *expr = parse_expr_cmp();
    while (match_token(TOKEN_AND_AND)) {
        SrcPos pos = token.pos;
        expr = new_expr_binary(pos, TOKEN_AND_AND, expr, parse_expr_cmp());
    }
    return expr;
}

Expr *parse_expr_or(void) {
    Expr *expr = parse_expr_and();
    while (match_token(TOKEN_OR_OR)) {
        SrcPos pos = token.pos;
        expr = new_expr_binary(pos, TOKEN_OR_OR, expr, parse_expr_and());
    }
    return expr;
}

Expr *parse_expr_ternary(void) {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_or();
    if (match_token(TOKEN_QUESTION)) {
        Expr *then_expr = parse_expr_ternary();
        expect_token(TOKEN_COLON);
        Expr *else_expr = parse_expr_ternary();
        expr = new_expr_ternary(pos, expr, then_expr, else_expr);
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
    SrcPos pos = token.pos;
    expect_token(TOKEN_LBRACE);
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(stmts, parse_stmt());
    }
    expect_token(TOKEN_RBRACE);
    return new_stmt_list(pos, stmts, buf_len(stmts));
}

Stmt *parse_init_stmt(Expr *left);

Stmt *parse_stmt_if(SrcPos pos) {
    expect_token(TOKEN_LPAREN);
    Expr *cond = parse_expr();
    Stmt *init = parse_init_stmt(cond);
    if (init) {
        if (match_token(TOKEN_SEMICOLON)) {
            cond = parse_expr();
        } else {
            cond = NULL;
        }
    }
    expect_token(TOKEN_RPAREN);
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
    return new_stmt_if(pos, init, cond, then_block, elseifs, buf_len(elseifs), else_block);
}

Stmt *parse_stmt_while(SrcPos pos) {
    Expr *cond = parse_paren_expr();
    return new_stmt_while(pos, cond, parse_stmt_block());
}

Stmt *parse_stmt_do_while(SrcPos pos) {
    StmtList block = parse_stmt_block();
    if (!match_keyword(while_keyword)) {
        fatal_error_here("Expected 'while' after 'do' block");
        return NULL;
    }
    Stmt *stmt = new_stmt_do_while(pos, parse_paren_expr(), block);
    expect_token(TOKEN_SEMICOLON);
    return stmt;
}

bool is_assign_op(void) {
    return TOKEN_FIRST_ASSIGN <= token.kind && token.kind <= TOKEN_LAST_ASSIGN;
}

Stmt *parse_init_stmt(Expr *left) {
    if (match_token(TOKEN_COLON_ASSIGN)) {
        if (left->kind != EXPR_NAME) {
            fatal_error_here(":= must be preceded by a name");
            return NULL;
        }
        return new_stmt_init(left->pos, left->name, NULL, parse_expr());
    } else if (match_token(TOKEN_COLON)) {
        if (left->kind != EXPR_NAME) {
            fatal_error_here(": must be preceded by a name");
            return NULL;
        }
        const char *name = left->name;
        Typespec *type = parse_type();
        Expr *expr = NULL;
        if (match_token(TOKEN_ASSIGN)) {
            expr = parse_expr();
        }
        return new_stmt_init(left->pos, name, type, expr);
    } else {
        return NULL;
    }
}

Stmt *parse_simple_stmt(void) {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr();
    Stmt *stmt = parse_init_stmt(expr);
    if (!stmt) {
        if (is_assign_op()) {
            TokenKind op = token.kind;
            next_token();
            stmt = new_stmt_assign(pos, op, expr, parse_expr());
        } else {
            stmt = new_stmt_expr(pos, expr);
        }
    }
    return stmt;
}

Stmt *parse_stmt_for(SrcPos pos) {
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
    Stmt *next = NULL;
    if (match_token(TOKEN_SEMICOLON)) {
        if (!is_token(TOKEN_RPAREN)) {
            next = parse_simple_stmt();
            if (next->kind == STMT_INIT) {
                error_here("Init statements not allowed in for-statement's next clause");
            }
        }
    }
    expect_token(TOKEN_RPAREN);
    return new_stmt_for(pos, init, cond, next, parse_stmt_block());
}

SwitchCase parse_stmt_switch_case(void) {
    Expr **exprs = NULL;
    bool is_default = false;
    while (is_keyword(case_keyword) || is_keyword(default_keyword)) {
        if (match_keyword(case_keyword)) {
            buf_push(exprs, parse_expr());
            while (match_token(TOKEN_COMMA)) {
                buf_push(exprs, parse_expr());
            }
        } else {
            assert(is_keyword(default_keyword));
            next_token();
            if (is_default) {
                error_here("Duplicate default labels in same switch clause");
            }
            is_default = true;
        }
        expect_token(TOKEN_COLON);
    }
    SrcPos pos = token.pos;
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE) && !is_keyword(case_keyword) && !is_keyword(default_keyword)) {
        buf_push(stmts, parse_stmt());
    }
    return (SwitchCase){exprs, buf_len(exprs), is_default, new_stmt_list(pos, stmts, buf_len(stmts))};
}

Stmt *parse_stmt_switch(SrcPos pos) {
    Expr *expr = parse_paren_expr();
    SwitchCase *cases = NULL;
    expect_token(TOKEN_LBRACE);
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(cases, parse_stmt_switch_case());
    }
    expect_token(TOKEN_RBRACE);
    return new_stmt_switch(pos, expr, cases, buf_len(cases));
}

Note parse_note(void);

Stmt *parse_stmt(void) {
    Notes notes = parse_notes();
    SrcPos pos = token.pos;
    Stmt *stmt = NULL;
    if (match_keyword(if_keyword)) {
        stmt = parse_stmt_if(pos);
    } else if (match_keyword(while_keyword)) {
        stmt = parse_stmt_while(pos);
    } else if (match_keyword(do_keyword)) {
        stmt = parse_stmt_do_while(pos);
    } else if (match_keyword(for_keyword)) {
        stmt = parse_stmt_for(pos);
    } else if (match_keyword(switch_keyword)) {
        stmt = parse_stmt_switch(pos);
    } else if (is_token(TOKEN_LBRACE)) {
        stmt = new_stmt_block(pos, parse_stmt_block());
    } else if (match_keyword(break_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_break(pos);
    } else if (match_keyword(continue_keyword)) {
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_continue(pos);
    } else if (match_keyword(return_keyword)) {
        Expr *expr = NULL;
        if (!is_token(TOKEN_SEMICOLON)) {
            expr = parse_expr();
        }
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_return(pos, expr);
    } else if (match_token(TOKEN_POUND)) {
        Note note = parse_note();
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_note(pos, note);
    } else {
        stmt = parse_simple_stmt();
        expect_token(TOKEN_SEMICOLON);
    }
    stmt->notes = notes;
    return stmt;
}

const char *parse_name(void) {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

EnumItem parse_decl_enum_item(void) {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    Expr *init = NULL;
    if (match_token(TOKEN_ASSIGN)) {
        init = parse_expr();
    }
    return (EnumItem){pos, name, init};
}

Decl *parse_decl_enum(SrcPos pos) {
    const char *name = NULL;
    if (is_token(TOKEN_NAME)) {
        name = parse_name();
    }
    Typespec *type = NULL;
    if (match_token(TOKEN_ASSIGN)) {
        type = parse_type();
    }
    expect_token(TOKEN_LBRACE);
    EnumItem *items = NULL;
    while (!is_token(TOKEN_RBRACE)) {
        buf_push(items, parse_decl_enum_item());
        if (!match_token(TOKEN_COMMA)) {
            break;
        }
    }
    expect_token(TOKEN_RBRACE);
    return new_decl_enum(pos, name, type, items, buf_len(items));
}

AggregateItem parse_decl_aggregate_item(void) {
    SrcPos pos = token.pos;
    const char **names = NULL;
    buf_push(names, parse_name());
    while (match_token(TOKEN_COMMA)) {
        buf_push(names, parse_name());
    }
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    expect_token(TOKEN_SEMICOLON);
    return (AggregateItem){pos, names, buf_len(names), type};
}

Decl *parse_decl_aggregate(SrcPos pos, DeclKind kind) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    const char *name = parse_name();
    if (match_token(TOKEN_SEMICOLON)) {
        Decl *decl = new_decl_aggregate(pos, kind, name, NULL, 0);
        decl->is_incomplete = true;
        return decl;
    } else {
        expect_token(TOKEN_LBRACE);
        AggregateItem *items = NULL;
        while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
            buf_push(items, parse_decl_aggregate_item());
        }
        expect_token(TOKEN_RBRACE);
        return new_decl_aggregate(pos, kind, name, items, buf_len(items));
    }
}

Decl *parse_decl_var(SrcPos pos) {
    const char *name = parse_name();
    if (match_token(TOKEN_ASSIGN)) {
        Expr *expr = parse_expr();
        expect_token(TOKEN_SEMICOLON);
        return new_decl_var(pos, name, NULL, expr);
    } else if (match_token(TOKEN_COLON)) {
        Typespec *type = parse_type();
        Expr *expr = NULL;
        if (match_token(TOKEN_ASSIGN)) {
            expr = parse_expr();
        }
        expect_token(TOKEN_SEMICOLON);
        return new_decl_var(pos, name, type, expr);
    } else {
        fatal_error_here("Expected : or = after var, got %s", token_info());
        return NULL;
    }
}

Decl *parse_decl_const(SrcPos pos) {
    const char *name = parse_name();
    Typespec *type = NULL;
    if (match_token(TOKEN_COLON)) {
        type = parse_type();
    }
    expect_token(TOKEN_ASSIGN);
    Expr *expr = parse_expr();
    expect_token(TOKEN_SEMICOLON);
    return new_decl_const(pos, name, type, expr);
}

Decl *parse_decl_typedef(SrcPos pos) {
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    Typespec *type = parse_type();
    expect_token(TOKEN_SEMICOLON);
    return new_decl_typedef(pos, name, type);
}

FuncParam parse_decl_func_param(void) {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    return (FuncParam){pos, name, type};
}

Decl *parse_decl_func(SrcPos pos) {
    const char *name = parse_name();
    expect_token(TOKEN_LPAREN);
    FuncParam *params = NULL;
    bool has_varargs = false;
    if (!is_token(TOKEN_RPAREN)) {
        buf_push(params, parse_decl_func_param());
        while (match_token(TOKEN_COMMA)) {
            if (match_token(TOKEN_ELLIPSIS)) {
                if (has_varargs) {
                    error_here("Multiple ellipsis in function declaration");
                }
                has_varargs = true;
            } else {
                if (has_varargs) {
                    error_here("Ellipsis must be last parameter in function declaration");
                }
                buf_push(params, parse_decl_func_param());
            }
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret_type = NULL;
    if (match_token(TOKEN_COLON)) {
        ret_type = parse_type();
    }
    StmtList block = {0};
    bool is_incomplete;
    if (match_token(TOKEN_SEMICOLON)) {
        is_incomplete = true;
    } else {
        block = parse_stmt_block();
        is_incomplete = false;
    }
    Decl *decl = new_decl_func(pos, name, params, buf_len(params), ret_type, has_varargs, block);
    decl->is_incomplete = is_incomplete;
    return decl;
}

NoteArg parse_note_arg(void) {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr();
    const char *name = NULL;
    if (match_token(TOKEN_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            fatal_error_here("Left operand of = in note argument must be a name");
        }
        name = expr->name;
        expr = parse_expr();
    }
    return (NoteArg){.pos = pos, .name = name, .expr = expr};
}

Note parse_note(void) {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    NoteArg *args = NULL;
    if (match_token(TOKEN_LPAREN)) {
        buf_push(args, parse_note_arg());
        while (match_token(TOKEN_COMMA)) {
            buf_push(args, parse_note_arg());
        }
        expect_token(TOKEN_RPAREN);
    }
    return new_note(pos, name, args, buf_len(args));
}
    
Notes parse_notes(void) {
    Note *notes = NULL;
    while (match_token(TOKEN_AT)) {
        buf_push(notes, parse_note());
    }
    return new_notes(notes, buf_len(notes));
}

Decl *parse_decl_note(SrcPos pos) {
    return new_decl_note(pos, parse_note());
}

Decl *parse_decl_import(SrcPos pos) {
    bool is_relative = false;
    if (match_token(TOKEN_DOT)) {
        is_relative = true;
    }
    const char **names = NULL;
    buf_push(names, token.name);
    expect_token(TOKEN_NAME);
    while (match_token(TOKEN_DOT)) {
        buf_push(names, token.name);
        expect_token(TOKEN_NAME);
    }
    bool import_all = false;
    ImportItem *items = NULL;
    if (match_token(TOKEN_LBRACE)) {
        while (!is_token(TOKEN_RBRACE)) {
            if (match_token(TOKEN_ELLIPSIS)) {
                import_all = true;
            } else {
                const char *name = parse_name();
                if (match_token(TOKEN_ASSIGN)) {
                    buf_push(items, (ImportItem){.name = parse_name(), .rename = name});
                } else {
                    buf_push(items, (ImportItem){.name = name});
                }
                if (!match_token(TOKEN_COMMA)) {
                    break;
                }
            }
        }
        expect_token(TOKEN_RBRACE);
    }
    return new_decl_import(pos, is_relative, names, buf_len(names), import_all, items, buf_len(items));
}

Decl *parse_decl_opt(void) {
    SrcPos pos = token.pos;
    if (match_keyword(enum_keyword)) {
        return parse_decl_enum(pos);
    } else if (match_keyword(struct_keyword)) {
        return parse_decl_aggregate(pos, DECL_STRUCT);
    } else if (match_keyword(union_keyword)) {
        return parse_decl_aggregate(pos, DECL_UNION);
    } else if (match_keyword(const_keyword)) {
        return parse_decl_const(pos);
    } else if (match_keyword(typedef_keyword)) {
        return parse_decl_typedef(pos);
    } else if (match_keyword(func_keyword)) {
        return parse_decl_func(pos);
    } else if (match_keyword(var_keyword)) {
        return parse_decl_var(pos);
    } else if (match_keyword(import_keyword)) {
        return parse_decl_import(pos);
    } else if (match_token(TOKEN_POUND)) {
        return parse_decl_note(pos);
    } else {
        return NULL;
    }
}

Decl *parse_decl(void) {
    Notes notes = parse_notes();
    Decl *decl = parse_decl_opt();
    if (!decl) {
        fatal_error_here("Expected declaration keyword, got %s", token_info());
    }
    decl->notes = notes;
    return decl;
}

Decls *parse_decls(void) {
    Decl **decls = NULL;
    while (!is_token(TOKEN_EOF)) {
        buf_push(decls, parse_decl());
    }
    return new_decls(decls, buf_len(decls));
}
