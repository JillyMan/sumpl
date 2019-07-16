void print_type(Typespec *type);
void print_expr(Expr* expr);

void print_type(Typespec *type) {
	switch (type->kind) {
	case TYPESPEC_NAME:
		printf("%s", type->name);
		break;
	case TYPESPEC_FUNC: {
		printf("(func (");
		for (Typespec **it = type->func.args; it != type->func.args + type->func.num_args; ++it) {
			printf(" ");
			print_type(*it);
		}
		printf(")");
		print_type(type->func.ret);
		printf(")");
		break;
	}
	case TYPESPEC_ARRAY:
		printf("(array ");
		print_type(type->array.base);
		printf(" ");
		print_expr(type->array.num_elems);
		printf(")");
		break;
	case TYPESPEC_PTR:
		printf("(ptr ");
		print_type(type->array.base);
		printf(")");
		break;
	default:
		assert(0);
	}
}

void print_expr(Expr *expr) {
	switch (expr->kind) {
	case EXPR_INT:
		printf("%"PRIu64, expr->int_lit.val);
		break;
	case EXPR_FLOAT:
		printf("%f", expr->float_lit.val);
		break;
	case EXPR_STR:
		printf("\"%s\"", expr->str_lit.val);
		break;
	case EXPR_NAME:
		printf("%s", expr->name);
		break;
	case EXPR_CAST:
		printf("(cast ");
		print_type(expr->cast.type);
		printf(" to ");
		print_expr(expr->cast.expr);
		printf(")");
		break;
		///
	case EXPR_CALL:
		printf("(func ");
		print_expr(expr->call.expr);
		for (Expr **it = expr->call.args; it != expr->call.args + expr->call.num_args; ++it) {
			printf(" ");
			print_expr(*it);
		}
		printf(")");
		break;
	case EXPR_INDEX:
		printf("(index ");
		print_expr(expr->index.expr);
		printf(" ");
		print_expr(expr->index.index);
		printf(")");
		break;
	case EXPR_FIELD:
		printf("(field ");
		print_expr(expr->field.expr);
		printf(".%s)", expr->field.name);
		break;
	case EXPR_UNARY:
		printf("(%c ", (char)expr->unary.op);
		print_expr(expr->unary.expr);
		printf(")");
		break;
	case EXPR_BINARY:
		printf("(%c ", expr->binary.op);
		print_expr(expr->binary.left);
		printf(" ");
		print_expr(expr->binary.right);
		printf(")");
		break;
	case EXPR_TERNARY:
		printf("(ternary ");
		print_expr(expr->ternary.cond);
		printf(")");
		printf("(then ");
		print_expr(expr->ternary.then_expr);
		printf(")");
		printf("(else_expr ");
		print_expr(expr->ternary.else_expr); printf(")");
		break;
	default:
		assert(0);
	}
}