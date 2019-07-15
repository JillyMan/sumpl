#pragma once
//Expression

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#if 0
Grammar:
	   TYPE = INT | DOUBLE | FLOAT | SHORT | CHAR
		   expr3 = TYPE | '(' expr0 ')'
		   expr2 = [-]expr3
		   expr1 = expr2([*/ ] expr2)*
		   expr0 = expr1([+-] expr1)*
		   expr = expr0;
#endif

	   int parse_expr();

	   int parse_expr3() {
		   int val;
		   if (is_token(TOKEN_INT)) {
			   val = token.value;
			   next_token();
		   }
		   else if (match_token('(')) {
			   val = parse_expr();
			   expec_token(')');
		   }
		   else {
			   fatal("expected integer or (, got [%s]", token_kind_str(token.kind));
		   }
		   return val;
	   }

	   int parse_expr2() {
		   int val = 0;
		   if (match_token('-')) {
			   int rval = parse_expr3();
			   val = -rval;
		   }
		   else {
			   int rval = parse_expr3();
			   val = rval;
		   }
		   return val;
	   }

	   int parse_expr1() {
		   int val = parse_expr2();
		   while (is_token('*') || is_token('/')) {
			   char op = token.kind;
			   next_token();
			   int rval = parse_expr2();

			   if (op == '*') {
				   val *= rval;
			   }
			   else {
				   assert(op == '/');
				   assert(rval != 0);
				   val /= rval;
			   }
		   }

		   return val;
	   }

	   int parse_expr0() {
		   int val = parse_expr1();
		   while (is_token('+') || is_token('-')) {
			   char op = token.kind;
			   next_token();
			   int rval = parse_expr1();
			   if (op == '+') {
				   val += rval;
			   }
			   else {
				   assert(op == '-');
				   val -= rval;
			   }
		   }
		   return val;
	   }

	   int parse_expr() {
		   return parse_expr0();
	   }

	   int parse_expr_str(const char* str) {
		   init_stream(str);
		   return parse_expr();
	   }

#define assert_expr(x) assert(parse_expr_str(#x) == (x))

	   void parser_test() {
		   assert_expr(1);
		   assert_expr(1 + 2);
		   assert_expr(4 * 3);
		   assert_expr((1 + 3) * 4);
		   assert_expr((1 + 3) / 2);
		   assert_expr((2 * 3) / 2 + (4 / 1));
	   }

#undef assert_expr
