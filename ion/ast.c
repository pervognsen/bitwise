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

StmtList stmt_list(SrcPos pos, Stmt **stmts, size_t num_stmts) {
    return (StmtList){pos, AST_DUP(stmts), num_stmts};
}

Typespec *typespec_new(TypespecKind kind, SrcPos pos) {
    Typespec *t = ast_alloc(sizeof(Typespec));
    t->kind = kind;
    t->pos = pos;
    return t;
}

Typespec *typespec_name(SrcPos pos, const char *name) {
    Typespec *t = typespec_new(TYPESPEC_NAME, pos);
    t->name = name;
    return t;

}

Typespec *typespec_ptr(SrcPos pos, Typespec *elem) {
    Typespec *t = typespec_new(TYPESPEC_PTR, pos);
    t->ptr.elem = elem;
    return t;
}

Typespec *typespec_array(SrcPos pos, Typespec *elem, Expr *size) {
    Typespec *t = typespec_new(TYPESPEC_ARRAY, pos);
    t->array.elem = elem;
    t->array.size = size;
    return t;
}

Typespec *typespec_func(SrcPos pos, Typespec **args, size_t num_args, Typespec *ret) {
    Typespec *t = typespec_new(TYPESPEC_FUNC, pos);
    t->func.args = AST_DUP(args);
    t->func.num_args = num_args;
    t->func.ret = ret;
    return t;
}

DeclSet *decl_set(Decl **decls, size_t num_decls) {
    DeclSet *declset = ast_alloc(sizeof(DeclSet));
    declset->decls = AST_DUP(decls);
    declset->num_decls = num_decls;
    return declset;
}

Decl *decl_new(DeclKind kind, SrcPos pos, const char *name) {
    Decl *d = ast_alloc(sizeof(Decl));
    d->kind = kind;
    d->pos = pos;
    d->name = name;
    return d;
}

Decl *decl_enum(SrcPos pos, const char *name, EnumItem *items, size_t num_items) {
    Decl *d = decl_new(DECL_ENUM, pos, name);
    d->enum_decl.items = AST_DUP(items);
    d->enum_decl.num_items = num_items;
    return d;
}

Decl *decl_aggregate(SrcPos pos, DeclKind kind, const char *name, AggregateItem *items, size_t num_items) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    Decl *d = decl_new(kind, pos, name);
    d->aggregate.items = AST_DUP(items);
    d->aggregate.num_items = num_items;
    return d;
}

Decl *decl_union(SrcPos pos, const char *name, AggregateItem *items, size_t num_items) {
    Decl *d = decl_new(DECL_UNION, pos, name);
    d->aggregate.items = AST_DUP(items);
    d->aggregate.num_items = num_items;
    return d;
}

Decl *decl_var(SrcPos pos, const char *name, Typespec *type, Expr *expr) {
    Decl *d = decl_new(DECL_VAR, pos, name);
    d->var.type = type;
    d->var.expr = expr;
    return d;
}

Decl *decl_func(SrcPos pos, const char *name, FuncParam *params, size_t num_params, Typespec *ret_type, StmtList block) {
    Decl *d = decl_new(DECL_FUNC, pos, name);
    d->func.params = AST_DUP(params);
    d->func.num_params = num_params;
    d->func.ret_type = ret_type;
    d->func.block = block;
    return d;
}

Decl *decl_const(SrcPos pos, const char *name, Expr *expr) {
    Decl *d = decl_new(DECL_CONST, pos, name);
    d->const_decl.expr = expr;
    return d;
}

Decl *decl_typedef(SrcPos pos, const char *name, Typespec *type) {
    Decl *d = decl_new(DECL_TYPEDEF, pos, name);
    d->typedef_decl.type = type;
    return d;
}

Expr *expr_new(ExprKind kind, SrcPos pos) {
    Expr *e = ast_alloc(sizeof(Expr));
    e->kind = kind;
    e->pos = pos;
    return e;
}

Expr *expr_sizeof_expr(SrcPos pos, Expr *expr) {
    Expr *e = expr_new(EXPR_SIZEOF_EXPR, pos);
    e->sizeof_expr = expr;
    return e;
}

Expr *expr_sizeof_type(SrcPos pos, Typespec *type) {
    Expr *e = expr_new(EXPR_SIZEOF_TYPE, pos);
    e->sizeof_type = type;
    return e;
}

Expr *expr_int(SrcPos pos, int int_val) {
    Expr *e = expr_new(EXPR_INT, pos);
    e->int_val = int_val;
    return e;
}

Expr *expr_float(SrcPos pos, double float_val) {
    Expr *e = expr_new(EXPR_FLOAT, pos);
    e->float_val = float_val;
    return e;
}

Expr *expr_str(SrcPos pos, const char *str_val) {
    Expr *e = expr_new(EXPR_STR, pos);
    e->str_val = str_val;
    return e;
}

Expr *expr_name(SrcPos pos, const char *name) {
    Expr *e = expr_new(EXPR_NAME, pos);
    e->name = name;
    return e;
}

Expr *expr_compound(SrcPos pos, Typespec *type, CompoundField *fields, size_t num_fields) {
    Expr *e = expr_new(EXPR_COMPOUND, pos);
    e->compound.type = type;
    e->compound.fields = AST_DUP(fields);
    e->compound.num_fields = num_fields;
    return e;
}

Expr *expr_cast(SrcPos pos, Typespec *type, Expr *expr) {
    Expr *e = expr_new(EXPR_CAST, pos);
    e->cast.type = type;
    e->cast.expr = expr;
    return e;
}

Expr *expr_call(SrcPos pos, Expr *expr, Expr **args, size_t num_args) {
    Expr *e = expr_new(EXPR_CALL, pos);
    e->call.expr = expr;
    e->call.args = AST_DUP(args);
    e->call.num_args = num_args;
    return e;
}

Expr *expr_index(SrcPos pos, Expr *expr, Expr *index) {
    Expr *e = expr_new(EXPR_INDEX, pos);
    e->index.expr = expr;
    e->index.index = index;
    return e;
}

Expr *expr_field(SrcPos pos, Expr *expr, const char *name) {
    Expr *e = expr_new(EXPR_FIELD, pos);
    e->field.expr = expr;
    e->field.name = name;
    return e;
}

Expr *expr_unary(SrcPos pos, TokenKind op, Expr *expr) {
    Expr *e = expr_new(EXPR_UNARY, pos);
    e->unary.op = op;
    e->unary.expr = expr;
    return e;
}

Expr *expr_binary(SrcPos pos, TokenKind op, Expr *left, Expr *right) {
    Expr *e = expr_new(EXPR_BINARY, pos);
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

Expr *expr_ternary(SrcPos pos, Expr *cond, Expr *then_expr, Expr *else_expr) {
    Expr *e = expr_new(EXPR_TERNARY, pos);
    e->ternary.cond = cond;
    e->ternary.then_expr = then_expr;
    e->ternary.else_expr = else_expr;
    return e;
}

Stmt *stmt_new(StmtKind kind, SrcPos pos) {
    Stmt *s = ast_alloc(sizeof(Stmt));
    s->kind = kind;
    s->pos = pos;
    return s;
}

Stmt *stmt_decl(SrcPos pos, Decl *decl) {
    Stmt *s = stmt_new(STMT_DECL, pos);
    s->decl = decl;
    return s;
}

Stmt *stmt_return(SrcPos pos, Expr *expr) {
    Stmt *s = stmt_new(STMT_RETURN, pos);
    s->expr = expr;
    return s;
}

Stmt *stmt_break(SrcPos pos) {
    return stmt_new(STMT_BREAK, pos);
}

Stmt *stmt_continue(SrcPos pos) {
    return stmt_new(STMT_CONTINUE, pos);
}

Stmt *stmt_block(SrcPos pos, StmtList block) {
    Stmt *s = stmt_new(STMT_BLOCK, pos);
    s->block = block;
    return s;
}

Stmt *stmt_if(SrcPos pos, Expr *cond, StmtList then_block, ElseIf *elseifs, size_t num_elseifs, StmtList else_block) {
    Stmt *s = stmt_new(STMT_IF, pos);
    s->if_stmt.cond = cond;
    s->if_stmt.then_block = then_block;
    s->if_stmt.elseifs = AST_DUP(elseifs);
    s->if_stmt.num_elseifs = num_elseifs;
    s->if_stmt.else_block = else_block;
    return s;
}

Stmt *stmt_while(SrcPos pos, Expr *cond, StmtList block) {
    Stmt *s = stmt_new(STMT_WHILE, pos);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}

Stmt *stmt_do_while(SrcPos pos, Expr *cond, StmtList block) {
    Stmt *s = stmt_new(STMT_DO_WHILE, pos);
    s->while_stmt.cond = cond;
    s->while_stmt.block = block;
    return s;
}
   
Stmt *stmt_for(SrcPos pos, Stmt *init, Expr *cond, Stmt *next, StmtList block) {
    Stmt *s = stmt_new(STMT_FOR, pos);
    s->for_stmt.init = init;
    s->for_stmt.cond = cond;
    s->for_stmt.next = next;
    s->for_stmt.block = block;
    return s;
}

Stmt *stmt_switch(SrcPos pos, Expr *expr, SwitchCase *cases, size_t num_cases) {
    Stmt *s = stmt_new(STMT_SWITCH, pos);
    s->switch_stmt.expr = expr;
    s->switch_stmt.cases = AST_DUP(cases);
    s->switch_stmt.num_cases = num_cases;
    return s;
}

Stmt *stmt_assign(SrcPos pos, TokenKind op, Expr *left, Expr *right) {
    Stmt *s = stmt_new(STMT_ASSIGN, pos);
    s->assign.op = op;
    s->assign.left = left;
    s->assign.right = right;
    return s;
}

Stmt *stmt_init(SrcPos pos, const char *name, Expr *expr) {
    Stmt *s = stmt_new(STMT_INIT, pos);
    s->init.name = name;
    s->init.expr = expr;
    return s;
}

Stmt *stmt_expr(SrcPos pos, Expr *expr) {
    Stmt *s = stmt_new(STMT_EXPR, pos);
    s->expr = expr;
    return s;
}

#undef AST_DUP
