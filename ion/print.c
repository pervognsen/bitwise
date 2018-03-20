void print_expr(Expr *expr);
void print_stmt(Stmt *stmt);
void print_decl(Decl *decl);

int indent;

void print_newline() {
    printf("\n%.*s", 2*indent, "                                                                      ");
}

void print_typespec(Typespec *type) {
    Typespec *t = type;
    switch (t->kind) {
    case TYPESPEC_NAME:
        printf("%s", t->name);
        break;
    case TYPESPEC_FUNC:
        printf("(func (");
        for (Typespec **it = t->func.args; it != t->func.args + t->func.num_args; it++) {
            printf(" ");
            print_typespec(*it);
        }
        printf(") ");
        print_typespec(t->func.ret);
        printf(")");
        break;
    case TYPESPEC_ARRAY:
        printf("(array ");
        print_typespec(t->array.elem);
        printf(" ");
        print_expr(t->array.size);
        printf(")");
        break;
    case TYPESPEC_PTR:
        printf("(ptr ");
        print_typespec(t->ptr.elem);
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
        print_typespec(e->cast.type);
        printf(" ");
        print_expr(e->cast.expr);
        printf(")");
        break;
    case EXPR_CALL:
        printf("(");
        print_expr(e->call.expr);
        for (Expr **it = e->call.args; it != e->call.args + e->call.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_INDEX:
        printf("(index ");
        print_expr(e->index.expr);
        printf(" ");
        print_expr(e->index.index);
        printf(")");
        break;
    case EXPR_FIELD:
        printf("(field ");
        print_expr(e->field.expr);
        printf(" %s)", e->field.name);
        break;
    case EXPR_COMPOUND:
        printf("(compound ");
        if (e->compound.type) {
            print_typespec(e->compound.type);
        } else {
            printf("nil");
        }
        for (Expr **it = e->compound.args; it != e->compound.args + e->compound.num_args; it++) {
            printf(" ");
            print_expr(*it);
        }
        printf(")");
        break;
    case EXPR_UNARY:
        printf("(%c ", e->unary.op);
        print_expr(e->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(%c ", e->binary.op);
        print_expr(e->binary.left);
        printf(" ");
        print_expr(e->binary.right);
        printf(")");
        break;
    case EXPR_TERNARY:
        printf("(if ");
        print_expr(e->ternary.cond);
        printf(" ");
        print_expr(e->ternary.if_true);
        printf(" ");
        print_expr(e->ternary.if_false);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_stmt_block(StmtBlock block, bool newlines) {
    printf("(block");
    indent++;
    for (Stmt **it = block.stmts; it != block.stmts + block.num_stmts; it++) {
        if (newlines) {
            print_newline();
        } else {
            printf(" ");
        }
        print_stmt(*it);
    }
    indent--;
    printf(")");
}

void print_stmt(Stmt *stmt) {
    Stmt *s = stmt;
    switch (s->kind) {
    case STMT_RETURN:
        printf("(return ");
        print_expr(s->return_stmt.expr);
        printf(")");
        break;
    case STMT_BREAK:
        printf("(break)");
        break;
    case STMT_CONTINUE:
        printf("(continue)");
        break;
    case STMT_BLOCK:
        print_stmt_block(s->block, true);
        break;
    case STMT_IF:
        printf("(if ");
        print_expr(s->if_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->if_stmt.then_block, true);
        for (ElseIf *it = s->if_stmt.elseifs; it != s->if_stmt.elseifs + s->if_stmt.num_elseifs; it++) {
            print_newline();
            printf("elseif ");
            print_expr(it->cond);
            print_newline();
            print_stmt_block(it->block, true);
        }
        if (s->if_stmt.else_block.num_stmts != 0) {
            print_newline();
            printf("else ");
            print_newline();
            print_stmt_block(s->if_stmt.else_block, true);
        }
        printf(")");
        indent--;
        break;
    case STMT_WHILE:
        printf("(while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block, true);
        indent--;
        printf(")");
        break;
    case STMT_DO_WHILE:
        printf("(do-while ");
        print_expr(s->while_stmt.cond);
        indent++;
        print_newline();
        print_stmt_block(s->while_stmt.block, true);
        indent--;
        printf(")");
        break;
    case STMT_FOR:
        printf("(for ");
        print_stmt_block(s->for_stmt.init, false);
        print_expr(s->for_stmt.cond);
        print_stmt_block(s->for_stmt.next, false);
        indent++;
        print_newline();
        print_stmt_block(s->for_stmt.block, true);
        indent--;
        break;
    case STMT_SWITCH:
        printf("(switch ");
        print_expr(s->switch_stmt.expr);
        indent++;
        for (SwitchCase *it = s->switch_stmt.cases; it != s->switch_stmt.cases + s->switch_stmt.num_cases; it++) {
            print_newline();
            printf("(case (%s", it->is_default ? " default" : "");
            for (Expr **expr = it->exprs; expr != it->exprs + it->num_exprs; expr++) {
                printf(" ");
                print_expr(*expr);
            }
            printf(" ) ");
            indent++;
            print_newline();
            print_stmt_block(it->block, true);
            indent--;
        }
        printf(")");
        indent--;
        break;
    case STMT_ASSIGN:
        printf("(%s ", token_kind_names[s->assign.op]);
        print_expr(s->assign.left);
        printf(" ");
        print_expr(s->assign.right);
        printf(")");
        break;
    case STMT_INIT:
        printf("(:= %s ", s->init.name);
        print_expr(s->init.expr);
        printf(")");
        break;
    case STMT_EXPR:
        print_expr(s->expr);
        break;
    default:
        assert(0);
        break;
    }
}

void print_aggregate_decl(Decl *decl) {
    Decl *d = decl;
    for (AggregateItem *it = d->aggregate.items; it != d->aggregate.items + d->aggregate.num_items; it++) {
        print_newline();
        printf("(");
        print_typespec(it->type);
        for (const char **name = it->names; name != it->names + it->num_names; name++) {
            printf(" %s", *name);
        }
        printf(")");
    }
}

void print_decl(Decl *decl) {
    Decl *d = decl;
    switch (d->kind) {
    case DECL_ENUM:
        printf("(enum %s", d->name);
        indent++;
        for (EnumItem *it = d->enum_decl.items; it != d->enum_decl.items + d->enum_decl.num_items; it++) {
            print_newline();
            printf("(%s ", it->name);
            if (it->expr) {
                print_expr(it->expr);
            } else {
                printf("nil");
            }
            printf(")");
        }
        indent--;
        printf(")");
        break;
    case DECL_STRUCT:
        printf("(struct %s", d->name);
        indent++;
        print_aggregate_decl(d);
        indent--;
        printf(")");
        break;
    case DECL_UNION:
        printf("(union %s", d->name);
        indent++;
        print_aggregate_decl(d);
        indent--;
        printf(")");
        break;
    case DECL_VAR:
        printf("(var %s ", d->name);
        print_typespec(d->var.type);
        printf(" ");
        print_expr(d->var.expr);
        printf(")");
        break;
    case DECL_CONST:
        printf("(const %s ", d->name);
        print_expr(d->var.expr);
        printf(")");
        break;
    case DECL_TYPEDEF:
        printf("(typedef %s ", d->name);
        print_typespec(d->typedef_decl.type);
        printf(")");
        break;
    case DECL_FUNC:
        printf("(func %s ", d->name);
        printf("(");
        for (FuncParam *it = d->func.params; it != d->func.params + d->func.num_params; it++) {
            printf(" %s", it->name);
            print_typespec(it->type);
        }
        printf(" ) ");
        print_typespec(d->func.ret_type);
        indent++;
        print_newline();
        print_stmt_block(d->func.block, true);
        indent--;
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_test() {
    // Expressions
    Expr *exprs[] = {
        expr_binary('+', expr_int(1), expr_int(2)),
        expr_unary('-', expr_float(3.14)),
        expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false")),
        expr_field(expr_name("person"), "name"),
        expr_call(expr_name("fact"), (Expr*[]){expr_int(42)}, 1),
        expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3)),
        expr_cast(typespec_ptr(typespec_name("int")), expr_name("void_ptr")),
        expr_compound(typespec_name("Vector"), (Expr*[]){expr_int(1), expr_int(2)}, 2),
    };
    for (Expr **it = exprs; it != exprs + sizeof(exprs)/sizeof(*exprs); it++) {
        print_expr(*it);
        printf("\n");
    }

    // Statements
    Stmt *stmts[] = {
        stmt_return(expr_int(42)),
        stmt_break(),
        stmt_continue(),
        stmt_block(
            (StmtBlock){
                (Stmt*[]){
                    stmt_break(),
                    stmt_continue()
                },
                2,
             }
        ),
        stmt_expr(expr_call(expr_name("print"), (Expr*[]){expr_int(1), expr_int(2)}, 2)),
        stmt_init("x", expr_int(42)),
        stmt_if(
            expr_name("flag1"),
            (StmtBlock){
                (Stmt*[]){
                    stmt_return(expr_int(1))
                },
                1,
            },
            (ElseIf[]){
                expr_name("flag2"),
                (StmtBlock){
                    (Stmt*[]){
                        stmt_return(expr_int(2))
                    },
                    1,
                }
            },
            1,
            (StmtBlock){
                (Stmt*[]){
                    stmt_return(expr_int(3))
                },
                1,
            }
        ),
        stmt_while(
            expr_name("running"),
            (StmtBlock){
                (Stmt*[]){
                    stmt_assign(TOKEN_ADD_ASSIGN, expr_name("i"), expr_int(16)),
                },
                1,
            }
        ),
        stmt_switch(
            expr_name("val"),
            (SwitchCase[]){
                {
                    (Expr*[]){expr_int(3), expr_int(4)},
                    2,
                    false,
                    (StmtBlock){
                        (Stmt*[]){stmt_return(expr_name("val"))},
                        1,
                    },
                },
                {
                    (Expr*[]){expr_int(1)},
                    1,
                    true,
                    (StmtBlock){
                        (Stmt*[]){stmt_return(expr_int(0))},
                        1,
                    },
                },
            },
            2
        ),
    };
    for (Stmt **it = stmts; it != stmts + sizeof(stmts)/sizeof(*stmts); it++) {
        print_stmt(*it);
        printf("\n");
    }
}
