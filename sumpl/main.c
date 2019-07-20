#pragma warning(disable: 4996)
#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#include "common.c"
#include "lex.c"
#include "ast.h"
#include "ast.c"
#include "print.c"
#include "parse.c"
#include "stack_vm.c"

void run_tests() {
	common_tests();
	lex_test();
	//ast_test();
	parse_test();
	stack_vm_test();
}

int main(int argc, char const **argv) {
	run_tests();
	return 0;
}