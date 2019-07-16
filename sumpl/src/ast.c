void *ast_malloc(size_t size) {
	assert(size);
	void *ptr = xmalloc(size);
	memset(ptr, 0, size);
	return ptr;
}

Typespec *new_typespec(TypespecKind kind) {
	Typespec *t = ast_malloc(sizeof(Typespec));
	t->kind = kind;
	return t;
}

Typespec *typespec_name(const char *names) {
	Typespec *type = new_typespec(TYPESPEC_NAME);
	type->name = names;
	return type;
}

Typespec *typespec_ptr(Typespec *base) {
	Typespec *type = new_typespec(TYPESPEC_PTR);
	type->array.base = base;
	return type;
}

Typespec *typespec_func(size_t num_args, Typespec **args, Typespec* ret) {
	Typespec *type = new_typespec(TYPESPEC_FUNC);
	type->func.args = args;
	type->func.num_args = num_args;
	type->func.ret = ret;
	return type;
}

Typespec *typespec_array(Typespec* base, Expr *size_expr) {
	Typespec *type = new_typespec(TYPESPEC_ARRAY);
	type->array.base = base;
	type->array.num_elems = size_expr;
	return type;
}

Expr *new_expr(ExprKind kind) {
	Expr *expr = ast_malloc(sizeof(Expr));
	expr->kind = kind;
	return expr;
}

Expr *new_expr_int(uint64_t int_val) {
	Expr *expr = new_expr(EXPR_INT);
	expr->int_lit.val = int_val;
	return expr;
}

Expr *new_expr_float(double float_val) {
	Expr *expr = new_expr(EXPR_FLOAT);
	expr->float_lit.val = float_val;
	return expr;
}

Expr *new_expr_str(const char *str_val) {
	Expr *expr = new_expr(EXPR_STR);
	expr->str_lit.val = str_val;
	return expr;
}
Expr *new_expr_name(const char* name){
	Expr *expr = new_expr(EXPR_NAME);
	expr->name = name;
	return expr;
}

Expr *new_expr_cast(Typespec *type, Expr *cast_expr) {
	Expr *expr = new_expr(EXPR_CAST);
	expr->cast.type = type;
	expr->cast.expr = cast_expr;
	return expr;
}

Expr *new_expr_unary(TokenKind op, Expr* operand)  {
	Expr *expr = new_expr(EXPR_UNARY);
	expr->unary.op = op;
	expr->unary.expr = operand;
	return expr;
}

Expr *new_expr_binary(TokenKind op, Expr* left, Expr* right) {
	Expr *expr = new_expr(EXPR_BINARY);
	expr->binary.left = left;
	expr->binary.op = op;
	expr->binary.right = right;
	return expr;
}

Expr *new_expr_ternary(Expr* cond, Expr* then_expr, Expr* else_expr) {
	Expr *expr = new_expr(EXPR_TERNARY);
	expr->ternary.cond = cond;
	expr->ternary.then_expr = then_expr;
	expr->ternary.else_expr = else_expr;
	return expr;
}

Expr *new_expr_call(Expr *call_expr, Expr **args, size_t num_args) {
	Expr *expr = new_expr(EXPR_CALL);
	expr->call.expr = call_expr;
	expr->call.args = args;
	expr->call.num_args = num_args;
	return expr;
}

Expr *new_expr_index(Expr *operand, Expr *index) {
	Expr *expr = new_expr(EXPR_INDEX);
	expr->index.expr = operand;
	expr->index.index = index;
	return expr;
}

Expr *new_expr_field(Expr *field_expr, const char* field) {
	Expr *expr = new_expr(EXPR_FIELD);
	expr->field.name = field;
	expr->field.expr = field_expr;
	return expr;
}

Stmt *new_stmt(StmtKind kind) {
	Stmt* stmt = ast_malloc(sizeof(Stmt));
	stmt->kind = kind;
	return stmt;
}

Stmt *new_stmt_return (Expr *expr) {
	Stmt* stmt = new_stmt(STMT_RETURN);
	stmt->expr = expr;
	return stmt;
}
Stmt *new_stmt_break () {
	return new_stmt(STMT_BREAK);
}

Stmt *new_stmt_continue() {
	return new_stmt(STMT_CONTINUE);
}

Stmt *new_stmt_block(StmtBlock block) {
	Stmt* stmt = new_stmt(STMT_BLOCK);
	stmt->block = block;
	return stmt;
}

Stmt *new_stmt_if(Expr *cond, StmtBlock then_block, size_t num_elseifs, ElseIf* elseifs, StmtBlock else_block) {
	Stmt* stmt = new_stmt(STMT_IF);
	stmt->if_stmt.cond = cond;
	stmt->if_stmt.then_block = then_block;
	stmt->if_stmt.num_elseifs = num_elseifs;
	stmt->if_stmt.elseifs = elseifs;
	stmt->if_stmt.else_block = else_block;
	return stmt;
}

Stmt *new_stmt_for(StmtBlock init, Expr *cond, StmtBlock update) {
	Stmt* stmt = new_stmt(STMT_FOR);
	stmt->for_stmt.init = init;
	stmt->for_stmt.cond = cond;
	stmt->for_stmt.update = init;
	return stmt;
}

Stmt *new_stmt_while(Expr *cond, Stmt* inner_stmt) {
	Stmt* stmt = new_stmt(STMT_WHILE);
	stmt->while_stmt.cond = cond;
	stmt->while_stmt.stmt = inner_stmt;
	return stmt;
}

Stmt *new_stmt_do(Expr *cond, Stmt* inner_stmt) {
	Stmt* stmt = new_stmt(STMT_DO);
	stmt->while_stmt.cond = cond;
	stmt->while_stmt.stmt = inner_stmt;
	return stmt;
}

Stmt *new_stmt_switch(Expr* expr, SwitchCasePattern *cases, size_t num_cases) {
	Stmt* stmt = new_stmt(STMT_SWITCH);
	stmt->switch_stmt.expr = expr;
	stmt->switch_stmt.cases = cases;
	stmt->switch_stmt.num_cases = num_cases;
	return stmt;
}

Stmt *new_stmt_assign(TokenKind op, Expr *left, Expr *right) {
	Stmt* stmt = new_stmt(STMT_ASSIGN);
	stmt->assign.op = op;
	stmt->assign.left = left;
	stmt->assign.right = right;
	return stmt;
}

Stmt *new_stmt_expr(Expr *expr) {
	Stmt* stmt = new_stmt(STMT_EXPR);
	stmt->expr = expr;
	return stmt;
}

Stmt *new_stmt_init(const char* name, Typespec *type, Expr *expr, bool is_undef) {
	Stmt* stmt = new_stmt(STMT_EXPR);
	stmt->init.name = name;
	stmt->init.type = type;
	stmt->init.expr = expr;
	stmt->init.is_undef = is_undef;
	return stmt;
}

Decl *new_decl(DeclKind kind) {
	Decl *decl = ast_malloc(sizeof(Decl));
	decl->kind = kind;
	return decl;
}

Decl *new_decl_func(const char* name, size_t num_params, FuncParam *params, Typespec *return_type, Stmt *stmt) {
	Decl *decl = new_decl(DECL_FUNC);
	decl->func.name = name;
	decl->func.num_params = num_params;
	decl->func.params = params;
	decl->func.return_type = return_type;
	decl->func.stmt = stmt;
	return decl;
}

Decl *new_decl_var(const char* name, Typespec *type, Expr *expr) {
	Decl *decl = new_decl(DECL_VAR);
	decl->name = name;
	decl->var.type = type;
	decl->var.expr = expr;
	return decl;
}