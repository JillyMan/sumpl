#define STACK_SIZE 1024

typedef struct VirtualMachine {	
	uint64_t *top;
	uint64_t *base;

	size_t num_locals;
	uint64_t *locals;

	uint64_t memory[STACK_SIZE];
} VirtualMachine;

typedef enum OpcodeKind {
	OPCODE_PUSH,
	OPCODE_ADD,
	OPCODE_MUL,
	OPCODE_DIV,
	OPCODE_SUB,
	OPCODE_PRINT,
} OpcodeKind;

typedef struct Opcode {
	OpcodeKind kind;	
	uint64_t operand;
} Opcode;

VirtualMachine vm;
Opcode opcode;

void reset_vm() {
	vm.top = &vm.memory[0];
	vm.base = &vm.memory[0];
}


#if 0
	lcvar_decl = ldloc.([0 - 9] * )
	locals_decl = locals { (type name),*  }
	

	opcode = 
			| locals_decl
			| lcvar
			| 'push'
		    | 'add'
		    | 'sub'
		    | 'mul'
		    | 'div'
		    | 'print'
		    | 'call'
	
	asm_opcode ::= opcode (operand)?
#endif

#define KEYWORD_NAME_ADD	"add"
#define KEYWORD_NAME_SUB	"sub"
#define KEYWORD_NAME_DIV	"div"
#define KEYWORD_NAME_MUL	"mul"
#define KEYWORD_NAME_INC	"inc"
#define KEYWORD_NAME_DEC	"dec"
#define KEYWORD_NAME_CAL	"dec"

#define KEYWORD_NAME_POP	"pop"
#define KEYWORD_NAME_PUSH	"push"

#define KEYWORD_NAME_PRINT	"print"

void asm_parse_operand() {
	next_token();
	if (is_token(TOKEN_INT)) {
		opcode.operand = token.int_val;
	} else {
		syntax_error("Expected operand.");
	}
}

void next_opcode() {
	if (is_token(TOKEN_NAME)) {		
		if (strcmp(token.str_val, KEYWORD_NAME_PUSH) == 0) {
			opcode.kind = OPCODE_PUSH;
			asm_parse_operand();
		} else if (strcmp(token.str_val, KEYWORD_NAME_SUB) == 0) {
			opcode.kind = OPCODE_ADD;
		} else if (strcmp(token.str_val, KEYWORD_NAME_SUB) == 0) {
			opcode.kind = OPCODE_SUB;
		} else if (strcmp(token.str_val, KEYWORD_NAME_DIV) == 0) {
			opcode.kind = OPCODE_DIV;
		} else if (strcmp(token.str_val, KEYWORD_NAME_MUL) == 0) {
			opcode.kind = OPCODE_MUL;
		} else if (strcmp(token.str_val, KEYWORD_NAME_PRINT) == 0) {
			opcode.kind = OPCODE_PRINT;
		}
	}

	next_token();
}

void vm_push(uint64_t value) {
	*vm.top = value;
	vm.top++;
}

uint64_t vm_pop() {
	return *(--vm.top);
}

uint64_t vm_peek() {
	return *(vm.top - 1);
}

#define BINARY_CASE(opcode, op) \
case opcode: { \
	uint64_t op1 = vm_pop(); \
	uint64_t op2 = vm_pop(); \
	vm_push(op1 op op2); \
	break; \
} \

void process() {	
	reset_vm();
	while (!is_token(TOKEN_EOF)) {
		next_opcode();
		switch (opcode.kind) {
			case OPCODE_PUSH:
				vm_push(opcode.operand);
				break;
			BINARY_CASE(OPCODE_ADD, +)
			BINARY_CASE(OPCODE_SUB, -)
			BINARY_CASE(OPCODE_MUL, *)
			case OPCODE_DIV: {
				uint64_t op1 = vm_pop();
				uint64_t op2 = vm_pop();
				assert(op2 == 0);
				vm_push(op1 / op2);
				break;
			}
			case OPCODE_PRINT: {
				uint64_t top = vm_peek();
				printf("stack top: %"PRIu64, top);
				break;
			}
			default:
				syntax_error("invalid operand '%s'", token.str_val);
			}
	}
}

#undef BINARY_CASE

void stack_vm_test() {
	init_stream("push 1 \
				 push 2 \
				 push 3	\
				 add    \
				 sub	\
				 print");
	process();
	
	//Commont test
	reset_vm();
	vm_push(1);
	vm_push(2);
	vm_pop();
	assert(vm_peek() == 1);
}