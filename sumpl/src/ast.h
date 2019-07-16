typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

typedef enum TypespecKind {
	TYPESPEC_NONE,
	TYPESPEC_NAME,
	TYPESPEC_PTR,
	TYPESPEC_FUNC,
	TYPESPEC_ARRAY,
	TYPESPEC_CONST,
} TypespecKind;

typedef struct Typespec {
	TypespecKind kind;
	union {
		const char *name;
		struct {
			size_t num_args;
			Typespec **args;
			Typespec* ret;
		} func;
		struct {
			Typespec *base;
			Expr *num_elems;
		} array;
	};
} Typespec;

typedef enum ExprKind {
	EXPR_NONE,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_STR,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_NAME,
	EXPR_INDEX,
	EXPR_FIELD,
	EXPR_COMPOUND,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_TERNARY,
} ExprKind;

struct Expr {
	ExprKind kind;
	union {
		const char *name;
		struct {
			uint64_t val;
			TokenMod mod;
		} int_lit;
		struct {
			double val;
		} float_lit;
		struct {
			const char *val;
			TokenMod mod;
		} str_lit;
		struct {
			Typespec *type;
			Expr *expr;
		} cast;
		struct {
			TokenKind op;
			Expr *expr;
		} unary;
		struct {
			TokenKind op;
			Expr *left;
			Expr *right;
		} binary;
		struct {
			Expr *cond;
			Expr *then_expr;
			Expr *else_expr;
		} ternary;
		struct {
			Expr *expr;
			Expr **args;
			size_t num_args;
		} call;
		struct {
			Expr *expr;
			Expr *index;
		} index;
		struct {
			Expr *expr;
			const char *name;
		} field;
	};
};

typedef enum DeclKind {
	DECL_NONE,
	DECL_ENUM,
	DECL_STRUCT,
	DECL_UNION,
	DECL_VAR,
	DEC_CONST,
	DECL_TYPEDEF,
	DECL_FUNC,
} DeclKind;

typedef struct EnumItem {
	const char* name;
	Typespec* type;
} EnumItem;

typedef struct AggregateItem {
	size_t num_names;
	const char **names;
	Typespec *type;
} AggregateItem;

typedef struct FuncParam {
	const char* name;
	Typespec *type;
} FuncParam;

struct Decl {
	DeclKind kind;
	const char *name;
	union {
		struct {
			const char *name;
			size_t num_enum_item;
			EnumItem *items;
		} enum_decl;
		struct {
			Typespec *type;
			Expr *expr;
		} var;
		struct {
			const char* name;
			size_t num_params;
			FuncParam* params;
			Typespec *return_type;
			Stmt* stmt;
		} func;
	};
};

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
	size_t num_stmts;
	Stmt **stmts;
} StmtBlock;

typedef struct ElseIf {
	Expr *cond;
	StmtBlock block;
} ElseIf;

typedef struct SwitchCasePattern {
	Expr *start;
	Expr *end;
} SwitchCasePattern;

typedef struct SwitchCase {
	SwitchCasePattern *patterns;
	size_t num_patterns;
	bool is_default;
	StmtBlock block;
} SwitchCase;

struct Stmt {
	StmtKind kind;
	union {
		Expr *expr;
		Decl *decl;
		StmtBlock block;
		struct {
			Expr *cond;
			StmtBlock then_block;
			size_t num_elseifs;
			ElseIf *elseifs;
			StmtBlock else_block;
		} if_stmt;
		struct {
			Expr *cond;
			Stmt *stmt;
		} while_stmt;
		struct {
			StmtBlock init;
			Expr *cond;
			StmtBlock update;
		} for_stmt;
		struct {
			TokenKind op;
			Expr *left;
			Expr *right;
		} assign;
		struct {
			Expr *expr;
			SwitchCasePattern *cases;
			size_t num_cases;
		} switch_stmt;
		struct {
			const char *name;
			Typespec *type;
			Expr* expr;
			bool is_undef;
		} init;
	};
};