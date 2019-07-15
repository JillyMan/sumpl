typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

typedef enum TypespecKind {
	TYPESPEC_NONE,
	TYPESPEC_NAME,
	TYPESPEC_PARENT,
	TYPESPEC_FUNC,
	TYPESPEC_ARRAY,
	TYPESPEC_POINTER,
} TypespecKind;

typedef struct FuncTypespec {
	BUF(Typespec **args);
	Typespec* ret;
} FuncTypespec;

typedef struct Typespec {
	TypespecKind kind;
	struct {
		const char *name;
		FuncTypespec *func;
		struct {
			Typespec *base_type;
			Expr *size;
		};
	};
} Typespec;

typedef enum DeclKind {
	DECL_NONE,
	DECL_ENUM,
	DECL_STRUCT,
	DECL_UNION,
	DECK_VAR,
	DEC_CONST,
	DECL_TYPEDEF,
	DECL_FUNC,
} DeclKind;

typedef struct EnumItem {
	const char* name;
	Typespec* type;
} EnumItem;

typedef struct AggregateItem {
	BUF(const char **names);
	Typespec *type;
} AggregateItem;

typedef struct FuncParam {
	const char* name;
	Typespec *type;
} FuncParam;

typedef struct FuncDecl {
	const char* name;
	BUF(FuncParam* params);
	Typespec *return_type;
	Stmt* stmt;
} FuncDecl;

struct Decl {
	DeclKind kind;
	const char *name;
	union {
		BUF(EnumItem *enum_items);
		BUF(AggregateItem *aggregate_items);
		struct {
			Typespec *type;
			Expr *expr;
		};
		FuncDecl func_decl;
	};
};

typedef enum ExprKind {
	EXPR_NONE,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_STR,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_NAME,
	EXPR_INDEC,
	EXPR_FIELD,
	EXPR_COMPOUND,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_TERNARY,
} ExprKind;

struct Expr {
	ExprKind kind;
	TokenKind op;
	union {
		//Literals
		uint64_t int_val;
		double float_val;
		const char *str_val;
		const char *name;

		//Compound literals
		struct {
			Typespec *compound_type;
			BUF(Expr **compound_args);
		};

		//Casts 
		struct {
			Typespec *cast_type;
			Expr *cast_expr;
		};

		struct {
			//unary
			Expr *operand_expr;
			union {
				//Call
				BUF(Expr **args);
				Expr *index;
				const char *field;
			};
		};

		struct {
			//binary
			Expr *left;
			Expr *right;
		};
		//Ternary
		struct {
			Expr *cond;
			Expr *then_expr;
			Expr *else_expr;
		};
	};
};
	
Expr *expr_alloc(ExprKind kind) {
	Expr *expr = xcalloc(1, sizeof(Expr));
	expr->kind = kind;
	return expr;
}

void expr_dealloc(Expr *expr) {
	assert(expr);
	free(expr);
}

Expr* expr_int(uint64_t int_val) {
	Expr *expr = expr_alloc(EXPR_INT);
	expr->int_val = int_val;
	return expr;
}

Expr* expr_float(double float_val) {
	Expr *expr = expr_alloc(EXPR_FLOAT);
	expr->float_val = float_val;
	return expr;
}

Expr* expr_str(const char *str_val) {
	Expr *expr = expr_alloc(EXPR_STR);
	expr->str_val = str_val;
	return expr;
}
Expr* expr_name(const char* name){
	Expr *expr = expr_alloc(EXPR_STR);
	expr->name = name;
	return NULL;
}

Expr* expr_cast(Typespec *type, Expr *cast_expr) {
	Expr *expr = expr_alloc(EXPR_CAST);
	expr->cast_type = type;
	expr->cast_expr = cast_expr;
	return expr;
}

Expr* expr_unary(TokenKind op, Expr* operand_expr)  {
	Expr *expr = expr_alloc(EXPR_UNARY);
	expr->op = op;
	expr->operand_expr = operand_expr;
	return expr;
}

Expr* expr_binary(TokenKind op, Expr* left, Expr* right) {
	Expr *expr = expr_alloc(EXPR_BINARY);
	expr->left = left;
	expr->op = op;
	expr->right = right;
	return expr;
}

Expr* expr_ternary(Expr* cond, Expr* then_expr, Expr* else_expr) {
	Expr *expr = expr_alloc(EXPR_TERNARY);
	expr->cond = cond;
	expr->then_expr = then_expr;
	expr->else_expr = else_expr;
	return expr;
}

#define UINT64_MINUS(x) ((uint64_t)x) |= (((uint64_t)1) << ((uint64_t)61))

uint64_t exec_expr(Expr *expr) {
	uint64_t lval, rval, val;

	switch (expr->kind)
	{
	case EXPR_INT:
		return expr->int_val;
		break;
	case EXPR_BINARY:
		assert(expr->left);
		assert(expr->right);
		lval = exec_expr(expr->left);
		rval = exec_expr(expr->right);
		break;
	case EXPR_UNARY:
		switch (expr->op) {
		case '+':
			val = +exec_expr(expr->operand_expr);
			break;
		case '-':
			val = exec_expr(expr->operand_expr);
			UINT64_MINUS(val);
			break;
		case '~':
			val = ~exec_expr(expr->operand_expr);
			break;
		case '!':
			val = !exec_expr(expr->operand_expr);
			break;
		case '&':
			fatal("not supported operator '&'");
			break;
		case '*':
			fatal("not supported operator '*'");
			break;
		}
		return val;
	default:
		break;
	}

	switch (expr->op) {
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
			val = expr->int_val;
		break;
	}

	return val;
}


Expr* parse_expr();

#define get_op_and_next_token() token.kind; next_token();
#define assert_expr(expr,res) assert(exec_expr((expr)) == (res))

Expr* parse_operand_expr() {
	Expr* expr = NULL;
	
	int op = token.kind;
	next_token();

	switch (op) {
	case TOKEN_INT:
		expr = expr_int(token.int_val);
		break;
	case TOKEN_FLOAT:
		expr = expr_float(token.float_val);
		break;
	case TOKEN_STRING:
		expr = expr_str(token.str_val);
		break;
	case TOKEN_NAME:
		expr = expr_name(token.name);
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
	Expr* expr = parse_operand_expr();
	//because i can't understand grammar for base_expr (.) check syntax.txt
	return expr;
}

bool is_unary_op() {
	return is_token('+') || is_token('-') ||
		is_token('&') || is_token('*');
}

Expr* parse_unary_expr() {
	Expr* expr;
	if (is_unary_op()) {
		int op = get_op_and_next_token();
		expr = expr_unary(op, parse_unary_expr());
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
		expr = expr_binary(op, expr, parse_unary_expr());
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
		expr = expr_binary(op, expr, parse_mul_expr());
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
		expr = expr_binary(op, expr, parse_add_expr());
	}

	return expr;
}

Expr* parse_and_expr() {
	Expr* expr = parse_cmp_expr();

	while (is_token(TOKEN_AND)) {
		int op = get_op_and_next_token();
		expr = expr_binary(op, expr, parse_cmp_expr());
	}

	return expr;
}

Expr* parse_or_expr() {
	Expr *expr = parse_and_expr();

	while (is_token(TOKEN_OR)) {
		int op = get_op_and_next_token();
		expr = expr_binary(op, expr, parse_and_expr());
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
			expr = expr_ternary(cond, then_expr, else_expr);
		}
	}

	return expr;
}

Expr* parse_expr() {
	return parse_ternary_expr();
}

void expr_test() {
	Expr *bin_expr = expr_binary('+',
								expr_binary('-', expr_int(4), expr_int(2)),
								expr_binary('*', expr_int(3), expr_int(2)));
	assert_expr(bin_expr, 8);

	init_stream("10");
	Expr *base_expr = parse_base_expr();
	assert(base_expr->int_val == 10);

	init_stream("+10");
	Expr *unary_expr = parse_unary_expr();
	assert(unary_expr->op == '+');
	assert(unary_expr->operand_expr->int_val == 10);

	init_stream("10 * 10 / 10");
	Expr *mul_expr = parse_mul_expr();
	assert_expr(mul_expr, 10);

	init_stream("10 + 10 - 1");
	Expr *add_expr = parse_add_expr();
	assert_expr(add_expr, 19);

	init_stream("10 == 10 || 10 != 1");
	Expr *cmp_expr = parse_cmp_expr();
	assert_expr(cmp_expr, 1);

	init_stream("(1+3)*(3*-2)");
	Expr *common_expr = parse_expr();
	int res = (int) exec_expr(common_expr);
	assert(res == 24);

	init_stream("1 != 2 && 2 >= 2 || 0");
	Expr *expr0 = parse_expr();
	assert_expr(expr0, 1);
}

typedef enum StmtKind {
	STMT_NONE,
	STMT_RETURN,
	STMT_BREAK,
	STMT_CONTINUE,
	STMT_BLOCK,
	STMT_IF,
	STMT_FOR,
	STMT_WHILE,
	STMT_DO,
	STMT_SWITCH,
	STMT_ASSIGN,
	STMT_AUTO_ASSIGN,
	STMT_EXPR,
} StmtKind;

typedef struct StmtBlock {
	BUF(Stmt **body);
} StmtBlock;

typedef struct ElseIf {
	Expr *cond;
	StmtBlock block;
} ElseIf;

typedef struct Case {
	BUF(Expr *exprs);
	StmtBlock block;
} Case;

struct Stmt {
	StmtKind kind;

	//just expr or union???
	union {
		Expr *if_expr;
		Expr *while_expr;
		Expr *lhs;
	};
	StmtBlock block;
	union {
		//If
		struct {
			BUF(ElseIf *elseifs);
			StmtBlock else_block;
		};
		//For 
		struct {
			StmtBlock for_init;
			StmtBlock for_update;
		};
		//Switch
		struct {
			BUF(Case *cases);
		};
		//Auto-asign
		struct {
			const char *var_name;
			Typespec *var_typespec;
			Expr* var_expr;
		};
		//Assignment/update
		struct {
			Expr *rhs;
		};
	};
};

Stmt* stmt_alloc() {
	Stmt* stmt = xcalloc(1, sizeof(Stmt));
	return stmt;
}

Decl* parse_decl() {
	assert(is_token(TOKEN_NAME));

	Decl *decl = xcalloc(1, sizeof(Decl));
	decl->name = token.name;
	next_token();
	Expr *expr = NULL;
	switch (token.kind) {
	case '=':
		decl->expr = parse_expr();
		break;
	case ':':
		next_token();
		if(strcmp(token.name, "int") == 0){
			next_token();
			if (is_token('=')) {
				decl->expr = parse_expr();
			}
		}
		break;
	}

	return decl;
}

#define assert_decl
#define assert_decl_expr(e, r) assert(exec_expr(e) == (r))

void stmt_test() {
	//init_stream("x = 10 + 20 + 30");
}

void ast_test() {
	expr_test();
	stmt_test();
}