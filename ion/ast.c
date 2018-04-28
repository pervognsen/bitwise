Arena ast_arena;

void *ast_alloc(size_t size) {
    assert(size != 0);
    void *ptr = arena_alloc(&ast_arena, size);
    memset(ptr, 0, size);
    return ptr;
}

void *ast_dup(const void *src, size_t size) {
    if (size == 0) {
        return NULL;
    }
    void *ptr = arena_alloc(&ast_arena, size);
    memcpy(ptr, src, size);
    return ptr;
}

#define AST_DUP(x) ast_dup(x, num_##x * sizeof(*x))

Note new_note(SrcPos pos, const char *name, NoteArg *args, size_t num_args) {
    return (Note){.pos = pos, .name = name, .args = AST_DUP(args), .num_args = num_args};
}

Notes new_notes(Note *notes, size_t num_notes) {
    return (Notes){AST_DUP(notes), num_notes};
}

StmtList new_stmt_list(SrcPos pos, Stmt **stmts, size_t num_stmts) {
    return (StmtList){pos, AST_DUP(stmts), num_stmts};
}

Typespec *new_typespec(TypespecKind kind, SrcPos pos) {
    Typespec *t = ast_alloc(sizeof(Typespec));
    t->kind = kind;
    t->pos = pos;
    return t;
}

Typespec *new_typespec_name(SrcPos pos, const char *name) {
    Typespec *t = new_typespec(TYPESPEC_NAME, pos);
    t->name = name;
    return t;

}

Typespec *new_typespec_ptr(SrcPos pos, Typespec *base) {
    Typespec *t = new_typespec(TYPESPEC_PTR, pos);
    t->base = base;
    return t;
}

Typespec *new_typespec_const(SrcPos pos, Typespec *base) {
    Typespec *t = new_typespec(TYPESPEC_CONST, pos);
    t->base = base;
    return t;
}

Typespec *new_typespec_array(SrcPos pos, Typespec *elem, Expr *size) {
    Typespec *t = new_typespec(TYPESPEC_ARRAY, pos);
    t->base = elem;
    t->num_elems = size;
    return t;
}

Typespec *new_typespec_func(SrcPos pos, Typespec **args, size_t num_args, Typespec *ret, bool has_varargs) {
    Typespec *t = new_typespec(TYPESPEC_FUNC, pos);
    t->func.args = AST_DUP(args);
    t->func.num_args = num_args;
    t->func.ret = ret;
    t->func.has_varargs = has_varargs;
    return t;
}

Decls *new_decls(Decl **decls, size_t num_decls) {
    Decls *d = ast_alloc(sizeof(Decls));
    d->decls = AST_DUP(decls);
    d->num_decls = num_decls;
    return d;
}

Decl *new_decl(DeclKind kind, SrcPos pos, const char *name) {
    Decl *d = ast_alloc(sizeof(Decl));
    d->kind = kind;
    d->pos = pos;
    d->name = name;
    return d;
}

Note *get_decl_note(Decl *decl, const char *name) {
    for (size_t i = 0; i < decl->notes.num_notes; i++) {
        Note *note = decl->notes.notes + i;
        if (note->name == name) {
            return note;
        }
    }
    return NULL;
}

bool is_decl_foreign(Decl *decl) {
    return get_decl_note(decl, foreign_name) != NULL;
}

Decl *new_decl_enum(SrcPos pos, const char *name, Typespec *type, EnumItem *items, size_t num_items) {
    Decl *d = new_decl(DECL_ENUM, pos, name);
    d->enum_decl.type = type;
    d->enum_decl.items = AST_DUP(items);
    d->enum_decl.num_items = num_items;
    return d;
}

Decl *new_decl_aggregate(SrcPos pos, DeclKind kind, const char *name, AggregateItem *items, size_t num_items) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    Decl *d = new_decl(kind, pos, name);
    d->aggregate.items = AST_DUP(items);
    d->aggregate.num_items = num_items;
    return d;
}

Decl *new_decl_union(SrcPos pos, const char *name, AggregateItem *items, size_t num_items) {
    Decl *d = new_decl(DECL_UNION, pos, name);
    d->aggregate.items = AST_DUP(items);
    d->aggregate.num_items = num_items;
    return d;
}

Decl *new_decl_var(SrcPos pos, const char *name, Typespec *type, Expr *expr) {
    Decl *d = new_decl(DECL_VAR, pos, name);
    d->var.type = type;
    d->var.expr = expr;
    return d;
}

Decl *new_decl_func(SrcPos pos, const char *name, FuncParam *params, size_t num_params, Typespec *ret_type, bool has_varargs, StmtList block) {
    Decl *d = new_decl(DECL_FUNC, pos, name);
    d->func.params = AST_DUP(params);
    d->func.num_params = num_params;
    d->func.ret_type = ret_type;
    d->func.has_varargs = has_varargs;
    d->func.block = block;
    return d;
}

Decl *new_decl_const(SrcPos pos, const char *name, Typespec *type, Expr *expr) {
    Decl *d = new_decl(DECL_CONST, pos, name);
    d->const_decl.type = type;
    d->const_decl.expr = expr;
    return d;
}

Decl *new_decl_typedef(SrcPos pos, const char *name, Typespec *type) {
    Decl *d = new_decl(DECL_TYPEDEF, pos, name);
    d->typedef_decl.type = type;
    return d;
}

Decl *new_decl_note(SrcPos pos, Note note) {
    Decl *d = new_decl(DECL_NOTE, pos, NULL);
    d->note = note;
    return d;
}

Decl *new_decl_import(SrcPos pos, bool is_relative, const char **names, size_t num_names, bool import_all, ImportItem *items, size_t num_items) {
    Decl *d = new_decl(DECL_IMPORT, pos, NULL);
    d->import.is_relative = is_relative;
    d->import.names = AST_DUP(names);
    d->import.num_names = num_names;
    d->import.import_all = import_all;
    d->import.items = AST_DUP(items);
    d->import.num_items = num_items;
    return d;
}

Expr *new_expr(ExprKind kind, SrcPos pos) {
    Expr *e = ast_alloc(sizeof(Expr));
    e->kind = kind;
    e->pos = pos;
    return e;
}

Expr *new_expr_paren(SrcPos pos, Expr *expr) {
    Expr *e = new_expr(EXPR_PAREN, pos);
    e->paren.expr = expr;
    return e;
}

Expr *new_expr_sizeof_expr(SrcPos pos, Expr *expr) {
    Expr *e = new_expr(EXPR_SIZEOF_EXPR, pos);
    e->sizeof_expr = expr;
    return e;
}

Expr *new_expr_sizeof_type(SrcPos pos, Typespec *type) {
    Expr *e = new_expr(EXPR_SIZEOF_TYPE, pos);
    e->sizeof_type = type;
    return e;
}

Expr *new_expr_typeof_expr(SrcPos pos, Expr *expr) {
    Expr *e = new_expr(EXPR_TYPEOF_EXPR, pos);
    e->typeof_expr  = expr;
    return e;
}

Expr *new_expr_typeof_type(SrcPos pos, Typespec *type) {
    Expr *e = new_expr(EXPR_TYPEOF_TYPE, pos);
    e->typeof_type = type;
    return e;
}

Expr *new_expr_alignof_expr(SrcPos pos, Expr *expr) {
    Expr *e = new_expr(EXPR_ALIGNOF_EXPR, pos);
    e->alignof_expr = expr;
    return e;
}

Expr *new_expr_alignof_type(SrcPos pos, Typespec *type) {
    Expr *e = new_expr(EXPR_ALIGNOF_TYPE, pos);
    e->alignof_type = type;
    return e;
}

Expr *new_expr_offsetof(SrcPos pos, Typespec *type, const char *name) {
    Expr *e = new_expr(EXPR_OFFSETOF, pos);
    e->offsetof_field.type = type;
    e->offsetof_field.name = name;
    return e;
}

Expr *new_expr_modify(SrcPos pos, TokenKind op, bool post, Expr *expr) {
    Expr *e = new_expr(EXPR_MODIFY, pos);
    e->modify.op = op;
    e->modify.post = post;
    e->modify.expr = expr;
    return e;
}

Expr *new_expr_int(SrcPos pos, unsigned long long val, TokenMod mod, TokenSuffix suffix) {
    Expr *e = new_expr(EXPR_INT, pos);
    e->int_lit.val = val;
    e->int_lit.mod = mod;
    e->int_lit.suffix = suffix;
    return e;
}

Expr *new_expr_float(SrcPos pos, double val, TokenSuffix suffix) {
    Expr *e = new_expr(EXPR_FLOAT, pos);
    e->float_lit.val = val;
    e->float_lit.suffix = suffix;
    return e;
}

Expr *new_expr_str(SrcPos pos, const char *val, TokenMod mod) {
    Expr *e = new_expr(EXPR_STR, pos);
    e->str_lit.val = val;
    e->str_lit.mod = mod;
    return e;
}

Expr *new_expr_name(SrcPos pos, const char *name) {
    Expr *e = new_expr(EXPR_NAME, pos);
    e->name = name;
    return e;
}

Expr *new_expr_compound(SrcPos pos, Typespec *type, CompoundField *fields, size_t num_fields) {
    Expr *e = new_expr(EXPR_COMPOUND, pos);
    e->compound.type = type;
    e->compound.fields = AST_DUP(fields);
    e->compound.num_fields = num_fields;
    return e;
}

Expr *new_expr_cast(SrcPos pos, Typespec *type, Expr *expr) {
    Expr *e = new_expr(EXPR_CAST, pos);
    e->cast.type = type;
    e->cast.expr = expr;
    return e;
}

Expr *new_expr_call(SrcPos pos, Expr *expr, Expr **args, size_t num_args) {
    Expr *e = new_expr(EXPR_CALL, pos);
    e->call.expr = expr;
    e->call.args = AST_DUP(args);
    e->call.num_args = num_args;
    return e;
}

Expr *new_expr_index(SrcPos pos, Expr *expr, Expr *index) {
    Expr *e = new_expr(EXPR_INDEX, pos);
    e->index.expr = expr;
    e->index.index = index;
    return e;
}

Expr *new_expr_field(SrcPos pos, Expr *expr, const char *name) {
    Expr *e = new_expr(EXPR_FIELD, pos);
    e->field.expr = expr;
    e->field.name = name;
    return e;
}

Expr *new_expr_unary(SrcPos pos, TokenKind op, Expr *expr) {
    Expr *e = new_expr(EXPR_UNARY, pos);
    e->unary.op = op;
    e->unary.expr = expr;
    return e;
}

Expr *new_expr_binary(SrcPos pos, TokenKind op, Expr *left, Expr *right) {
    Expr *e = new_expr(EXPR_BINARY, pos);
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

Expr *new_expr_ternary(SrcPos pos, Expr *cond, Expr *then_expr, Expr *else_expr) {
    Expr *e = new_expr(EXPR_TERNARY, pos);
    e->ternary.cond = cond;
    e->ternary.then_expr = then_expr;
    e->ternary.else_expr = else_expr;
    return e;
}

Note *get_stmt_note(Stmt *stmt, const char *name) {
    for (size_t i = 0; i < stmt->notes.num_notes; i++) {
        Note *note = stmt->notes.notes + i;
        if (note->name == name) {
            return note;
        }
    }
    return NULL;
}

Stmt *new_stmt(StmtKind kind, SrcPos pos) {
    Stmt *s = ast_alloc(sizeof(Stmt));
    s->kind = kind;
    s->pos = pos;
    return s;
}

Stmt *new_stmt_note(SrcPos pos, Note note) {
    Stmt *s = new_stmt(STMT_NOTE, pos);
    s->note = note;
    return s;
}

Stmt *new_stmt_decl(SrcPos pos, Decl *decl) {
    Stmt *s = new_stmt(STMT_DECL, pos);
    s->decl = decl;
    return s;
}

Stmt *new_stmt_return(SrcPos pos, Expr *expr) {
    Stmt *s = new_stmt(STMT_RETURN, pos);
    s->expr = expr;
    return s;
}

Stmt *new_stmt_break(SrcPos pos) {
    return new_stmt(STMT_BREAK, pos);
}

Stmt *new_stmt_continue(SrcPos pos) {
    return new_stmt(STMT_CONTINUE, pos);
}

Stmt *new_stmt_block(SrcPos pos, StmtList block) {
    Stmt *s = new_stmt(STMT_BLOCK, pos);
    s->block = block;
    return s;
}

Stmt *new_stmt_if(SrcPos pos, Stmt *init, Expr *cond, StmtList then_block, ElseIf *elseifs, size_t num_elseifs, StmtList else_block) {
    Stmt *s = new_stmt(STMT_IF, pos);
    s->if_stmt.init = init;
    s->if_stmt.cond = cond;
    s->if_stmt.then_block = then_block;
    s->if_stmt.elseifs = AST_DUP(elseifs);
    s->if_stmt.num_elseifs = num_elseifs;
    s->if_stmt.else_block = else_block;
    return s;
}

Stmt *new_stmt_while(SrcPos pos, Expr *cond, StmtList block) {
    Stmt *s = new_stmt(STMT_WHILE, pos);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}

Stmt *new_stmt_do_while(SrcPos pos, Expr *cond, StmtList block) {
    Stmt *s = new_stmt(STMT_DO_WHILE, pos);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}
   
Stmt *new_stmt_for(SrcPos pos, Stmt *init, Expr *cond, Stmt *next, StmtList block) {
    Stmt *s = new_stmt(STMT_FOR, pos);
    s->for_stmt.init = init;
    s->for_stmt.cond = cond;
    s->for_stmt.next = next;
    s->for_stmt.block = block;
    return s;
}

Stmt *new_stmt_switch(SrcPos pos, Expr *expr, SwitchCase *cases, size_t num_cases) {
    Stmt *s = new_stmt(STMT_SWITCH, pos);
    s->switch_stmt.expr = expr;
    s->switch_stmt.cases = AST_DUP(cases);
    s->switch_stmt.num_cases = num_cases;
    return s;
}

Stmt *new_stmt_assign(SrcPos pos, TokenKind op, Expr *left, Expr *right) {
    Stmt *s = new_stmt(STMT_ASSIGN, pos);
    s->assign.op = op;
    s->assign.left = left;
    s->assign.right = right;
    return s;
}

Stmt *new_stmt_init(SrcPos pos, const char *name, Typespec *type, Expr *expr) {
    Stmt *s = new_stmt(STMT_INIT, pos);
    s->init.name = name;
    s->init.type = type;
    s->init.expr = expr;
    return s;
}

Stmt *new_stmt_expr(SrcPos pos, Expr *expr) {
    Stmt *s = new_stmt(STMT_EXPR, pos);
    s->expr = expr;
    return s;
}

#undef AST_DUP
