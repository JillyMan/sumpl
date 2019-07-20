#define pause() //system("PAUSE")

Expr* parse_expr();

Expr* parse_operand() {
	Expr* expr = NULL;

	int op = token.kind;
	next_token();

	switch (op) {
	case TOKEN_INT:
		expr = new_expr_int(token.int_val);
		break;
	case TOKEN_FLOAT:
		expr = new_expr_float(token.float_val);
		break;
	case TOKEN_STRING:
		expr = new_expr_str(token.str_val);
		break;
	case TOKEN_NAME:
		expr = new_expr_name(token.name);
		break;
	case '(':
		expr = parse_expr();
		expec_token(')');
		break;
	default:
		fatal("Expected operand expresion");
		break;
	}

	return expr;
}

Expr* parse_base_expr() {
	Expr* expr = parse_operand();
	return expr;
}

bool is_unary_op() {
	return is_token('+') || is_token('-') ||
		is_token('&') || is_token('*');
}

#define get_op_and_next_token() token.kind; next_token();

Expr* parse_unary_expr() {
	Expr* expr;
	if (is_unary_op()) {
		int op = get_op_and_next_token();
		expr = new_expr_unary(op, parse_unary_expr());
	}
	else {
		expr = parse_base_expr();
	}

	return expr;
}

bool is_mul_op() {
	return is_token('*') || is_token('/') || is_token('%') ||
		is_token('&') || is_token(TOKEN_LSHIFT) ||
		is_token(TOKEN_RSHIFT);
}

Expr* parse_mul_expr() {
	Expr *expr = parse_unary_expr();

	while (is_mul_op()) {
		int op = get_op_and_next_token();
		expr = new_expr_binary(op, expr, parse_unary_expr());
	}

	return expr;
}

bool is_add_op() {
	return is_token('+') || is_token('-') ||
		is_token('|') || is_token('^');
}

Expr* parse_add_expr() {
	Expr* expr = parse_mul_expr();

	while (is_add_op()) {
		int op = get_op_and_next_token();
		expr = new_expr_binary(op, expr, parse_mul_expr());
	}

	return expr;
}

bool is_cmp_op() {
	return is_token(TOKEN_EQ) || is_token(TOKEN_NOTEQ) ||
		is_token('<') || is_token(TOKEN_LTEQ) ||
		is_token('>') || is_token(TOKEN_GTEQ);
}

Expr* parse_cmp_expr() {
	Expr* expr = parse_add_expr();

	while (is_cmp_op()) {
		int op = get_op_and_next_token();
		expr = new_expr_binary(op, expr, parse_add_expr());
	}

	return expr;
}

Expr* parse_and_expr() {
	Expr* expr = parse_cmp_expr();

	while (is_token(TOKEN_AND)) {
		int op = get_op_and_next_token();
		expr = new_expr_binary(op, expr, parse_cmp_expr());
	}

	return expr;
}

Expr* parse_or_expr() {
	Expr *expr = parse_and_expr();

	while (is_token(TOKEN_OR)) {
		int op = get_op_and_next_token();
		expr = new_expr_binary(op, expr, parse_and_expr());
	}

	return expr;
}

Expr* parse_ternary_expr() {
	Expr* cond = parse_or_expr();
	Expr* expr = cond;

	while (match_token('?')) {
		Expr *then_expr = parse_ternary_expr();
		if (expec_token(':')) {
			Expr *else_expr = parse_ternary_expr();
			expr = new_expr_ternary(cond, then_expr, else_expr);
		}
	}

	return expr;
}

Expr* parse_expr() {
	return parse_ternary_expr();
}

#define UINT64_MINUS(x) (-(int64_t)x)

uint64_t exec_expr(Expr *expr) {
	uint64_t lval, rval, val;
	char op = 0;

	switch (expr->kind)
	{
	case EXPR_INT:
		return expr->int_lit.val;
		break;
	case EXPR_BINARY:
		assert(expr->binary.left);
		assert(expr->binary.right);
		op = expr->binary.op;
		lval = exec_expr(expr->binary.left);
		rval = exec_expr(expr->binary.right);
		break;
	case EXPR_UNARY: {
		val = exec_expr(expr->unary.expr);
		switch (expr->unary.op) {
		case '+':
			val = +val;
			break;
		case '-':
			val = UINT64_MINUS(val);
			break;
		case '~':
			val = ~val;
			break;
		case '!':
			val = !val;
			break;
		case '&':
			fatal("not supported operator '&'");
			break;
		case '*':
			fatal("not supported operator '*'");
			break;
		}
		return val;
	}
	default:
		break;
	}

	switch (op) {
	case '+':
		val = lval + rval;
		break;
	case '-':
		val = lval - rval;
		break;
	case '*':
		val = lval * rval;
		break;
	case '/':
		val = lval / rval;
		break;
	case '%':
		val = lval % rval;
		break;
	case '&':
		val = lval & rval;
		break;
	case '>':
		val = lval > rval;
		break;
	case '<':
		val = lval < rval;
		break;
	case TOKEN_LSHIFT:
		val = lval << rval;
		break;
	case TOKEN_RSHIFT:
		val = lval >> rval;
		break;
	case TOKEN_EQ:
		val = lval == rval;
		break;
	case TOKEN_NOTEQ:
		val = lval != rval;
		break;
	case TOKEN_LTEQ:
		val = lval <= rval;
		break;
	case TOKEN_GTEQ:
		val = lval >= rval;
		break;
	case TOKEN_OR:
		val = lval || rval;
		break;
	case TOKEN_AND:
		val = lval && rval;
		break;
	default:
		val = expr->int_lit.val;
		break;
	}

	return val;
}

#define assert_expr(expr,res) assert(exec_expr(expr) == res)
#define assert_bin_expr(expr) init_stream(#expr); assert_expr(parse_expr(), expr);

void print_expr_tests() {
	Expr *exprs[] = {
		new_expr_binary('+', new_expr_int(10), new_expr_int(20)),
		new_expr_unary('-', new_expr_float(3.14)),
		new_expr_ternary(new_expr_binary(TOKEN_EQ, new_expr_int(10), new_expr_int(10)),
		new_expr_int(30), new_expr_int(24)),
		new_expr_field(new_expr_name("person"), "name"),
		new_expr_call(new_expr_name("fact"), (Expr*[]) { new_expr_int(32) }, 1),
		new_expr_cast(typespec_ptr(typespec_name("int")), new_expr_name("foo_ptr")),
	};

	for (Expr **it = exprs; it != exprs + array_size(exprs); ++it) {
		print_expr(*it);
		printf("\n");
	}

	pause();
}

void expr_test() {
	print_expr_tests();

	Expr *bin_expr = new_expr_binary('+',
		new_expr_binary('-', new_expr_int(4), new_expr_int(2)),
		new_expr_binary('*',
			new_expr_int(3),
			new_expr_unary('-', new_expr_int(10))));
	assert_expr(bin_expr, -28);

	init_stream("10");
	Expr *base_expr = parse_base_expr();
	assert(base_expr->int_lit.val == 10);

	init_stream("&10");
	Expr *unary_expr = parse_unary_expr();
	assert(unary_expr->unary.op == '&');
	assert(unary_expr->unary.expr->int_lit.val == 10);

	assert_bin_expr(10 * 10 / 10);
	assert_bin_expr(10 + 10 - 1);
	assert_bin_expr(10 == 10 || 10 != 1);
	assert_bin_expr((1 + 3)*(3 * -2));
	assert_bin_expr(1 != 2 && 2 >= 2 || 0);
}


Stmt *stmt_parse() {
	Stmt *stmt = NULL;
	return stmt;
}

Decl* decl_parse() {
	Decl *decl = NULL;
	return decl;
}

void parse() {
	Decl *main_func = NULL;
	while (!is_token(TOKEN_EOF)) {
	}
}

void parse_test() {
	expr_test();
}

#undef UINT64_MINUS
#undef assert_expr
#undef assert_bin_expr
#undef get_op_and_next_token
#undef pause