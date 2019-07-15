typedef enum TokenKind {
	TOKEN_EOF = 0,
	TOKEN_LAST_CHAR = 127,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_STRING,
	TOKEN_NAME,
	TOKEN_OPERATOR,
	TOKEN_LSHIFT,
	TOKEN_RSHIFT,
	TOKEN_EQ,
	TOKEN_NOTEQ,
	TOKEN_LTEQ,
	TOKEN_GTEQ,
	TOKEN_AND,
	TOKEN_OR,
	TOKEN_COLON_ASSIGN,
	TOKEN_ADD_ASSIGN,
	TOKEN_MINUS_ASSIGN,
	TOKEN_AND_ASSIGN,
	TOKEN_OR_ASSIGN,
	TOKEN_XOR_ASSIGN,
	TOKEN_LSHIFT_ASSIGN,
	TOKEN_RSHIFT_ASSIGN,
	TOKEN_MULT_ASSIGN,
	TOKEN_DIV_ASSIGN,
	TOKEN_MOD_ASSIGN,
	TOKEN_INC,
	TOKEN_DEC,
	//...
} TokenKind;

typedef enum TokenMod {
	TOKENMOD_NONE,
	TOKENMOD_HEX,
	TOKENMOD_BIN,
	TOKENMOD_OCT,
	TOKENMOD_CHAR,
} TokenMod;

typedef struct Token {
	TokenKind kind;
	TokenMod mod;
	const char*  start;
	const char*  end;

	union {
		uint64_t int_val;
		double  float_val;
		const char* str_val;
		const char* name;
	};
} Token;

Token token;
const char* stream;

const char *keyword_if;
const char *keyword_for;
const char *keyword_while;

void init_keywords() {
	keyword_if = str_intern("if");
	keyword_for = str_intern("for");
	keyword_while = str_intern("while");
}

size_t copy_token_kind_str(char* dest, size_t dest_size, TokenKind kind) {
	size_t n = 0;
	switch (kind) {
	case 0:
		n = snprintf(dest, dest_size, "end of file");
		break;
	case TOKEN_INT:
		n = snprintf(dest, dest_size, "integer");
		break;
	case TOKEN_FLOAT:
		n = snprintf(dest, dest_size, "float");
		break;
	case TOKEN_NAME:
		n = snprintf(dest, dest_size, "name");
		break;
	default:
		if (kind < 128 && isprint(kind)) {
			n = snprintf(dest, dest_size, "%c", kind);
		}
		else {
			n = snprintf(dest, dest_size, "<ASCII %d>", kind);
		}
		break;
	}
	return n;
}

const char *token_kind_str(TokenKind kind) {
	static char buf[256];
	size_t n = copy_token_kind_str(buf, sizeof(buf), kind);
	assert(n + 1 <= sizeof(buf));
	return buf;
}

const uint8_t char_to_digit[] = {
	['0'] = 0,
	['1'] = 1,
	['2'] = 2,
	['3'] = 3,
	['4'] = 4,
	['5'] = 5,
	['6'] = 6,
	['7'] = 7,
	['8'] = 8,
	['9'] = 9,
	['a'] = 10,['A'] = 10,
	['b'] = 11,['B'] = 11,
	['c'] = 12,['C'] = 12,
	['d'] = 13,['D'] = 13,
	['e'] = 14,['E'] = 14,
	['f'] = 15,['F'] = 15,
};

static inline void stream_inc() {
	stream++;
}

void scan_int() {
	uint64_t base = 10;

	if (*stream == '0') {
		stream++;
		if (tolower(*stream) == 'x') {
			token.mod = TOKENMOD_HEX;
			stream++;
			base = 16;
		}
		else if (tolower(*stream) == 'b') {
			token.mod = TOKENMOD_BIN;
			stream++;
			base = 2;
		}
		else if (isdigit(*stream)) {
			token.mod = TOKENMOD_OCT;
			base = 8;
		}
	}

	uint64_t val = 0;
	for (;;) {
		uint64_t digit = char_to_digit[*stream];
		if (digit == 0 && *stream != '0') {
			break;
		}

		if (digit > base) {
			syntax_error("Digit '%c' out of range for base %"PRIu64, *stream, base);
			digit = 0;
		}

		if (val > (UINT64_MAX - digit) / base) {
			syntax_error("integer literal overflow");
			while (isdigit(*stream)) {
				stream++;
			}
		}

		val = val * base + digit;
		stream++;
	}
	token.kind = TOKEN_INT;
	token.int_val = val;
}

void scan_float() {
	const char* start = stream;
	while (isdigit(*stream)) {
		stream++;
	}

	if (*stream == '.') {
		stream++;
	}

	while (isdigit(*stream)) {
		stream++;
	}

	if (tolower(*stream) == 'e') {
		stream++;
		if (*stream == '+' || *stream == '-') {
			stream++;
		}
		if (!isdigit(*stream)) {
			syntax_error("Epected digit after exponent in float literal, found %c.", *stream);
		}
		else {
			while (isdigit(*stream)) {
				stream++;
			}
		}
	}

	double val = strtod(start, NULL);

	if (val == HUGE_VAL || val == -HUGE_VAL) {
		syntax_error("Float literal overflow.");
	}
	token.kind = TOKEN_FLOAT;
	token.float_val = val;
}

char escape_to_char[256] = {
	['n'] = '\n',
	['r'] = '\r',
	['t'] = '\t',
	['v'] = '\v',
	['b'] = '\b',
	['a'] = '\a',
	['0'] = 0,
};

void scan_char() {
	assert(*stream == '\'');

	stream++;

	char val = 0;
	if (*stream == '\'') {
		syntax_error("Char literal can't be empty.");
	}
	else if (*stream == '\n') {
		syntax_error("Char literal can't contain newline.");
	}
	else if (*stream == '\\') {
		stream++;
		val = escape_to_char[*stream];
		if (val == 0 && *stream != '0') {
			syntax_error("Invalid char literal escape '\\%c'", *stream);
		}
	}
	else {
		val = *stream;
	}

	stream++;

	if (*stream != '\'') {
		syntax_error("Expected closing char quote, got '%c'.", *stream);
	}
	else {
		stream++;
	}

	token.kind = TOKEN_INT;
	token.int_val = val;
	token.mod = TOKENMOD_CHAR;
}

void scan_str() {
	assert(*stream == '"');

	stream++;
	char* buf = NULL;
	while (*stream && *stream != '"') {
		char val = *stream;
		if (val == '\n') {
			syntax_error("String literal can't contain newline.");
		}
		else if (val == '\\') {
			stream++;
			val = escape_to_char[*stream];
			if (val == 0 && *stream != '0') {
				syntax_error("Invalid string literal escape '\\%c'", *stream);
			}
		}
		buf_push(buf, val);
		stream++;
	}

	if (*stream != '"' || !(*stream)) {
		syntax_error("Expected double quote '\"', in string literal.");
	}

	buf_push(buf, 0);

	token.kind = TOKEN_STRING;
	token.str_val = buf;//str_intern_range(token.start + 1, stream);
	stream++;
}

#define LEX_CASE2(c, c1, k1, c2, k2)\
case c: \
	token.kind = *stream++; \
	if (*stream == c1) { \
		token.kind = k1; \
		stream++; \
	} \
	else if (*stream == c2) { \
		token.kind = k2; \
		stream++; \
	}\
	break;\

#define LEX_CASE1(c, c1, k1) \
case c: \
	token.kind = *stream++; \
	if (*stream == c1) { \
		token.kind = k1; \
		stream++; \
	} \
	break;\

void next_token() {
top:
	token.start = stream;
	switch (*stream) {
	case ' ': case '\n': case '\r': case '\t': case '\v':
		while (isspace(*stream)) {
			stream++;
		}
		goto top;
		break;
	case '\'':
		scan_char();
		break;
	case '"':
		scan_str();
		break;
	case '.':
		scan_float();
		break;
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
		const char* bookmark = stream;
		while (isdigit(*bookmark)) {
			bookmark++;
		}

		if (*bookmark == '.' || tolower(*bookmark) == 'e') {
			scan_float();
		}
		else {
			scan_int();
		}

		break;
	}

	case '<':
		token.kind = *stream++;
		if (*stream == '<') {
			token.kind = TOKEN_LSHIFT;
			stream++;
			if (*stream == '=') {
				token.kind = TOKEN_LSHIFT_ASSIGN;
			}
		}
		else if (*stream == '=') {
			token.kind = TOKEN_LTEQ;
		}
		*stream++;
		break;
	case '>':
		token.kind = *stream++;
		if (*stream == '>') {
			token.kind = TOKEN_RSHIFT;
			stream++;
			if (*stream == '=') {
				token.kind = TOKEN_RSHIFT_ASSIGN;
			}
		}
		else if (*stream == '=') {
			token.kind = TOKEN_GTEQ;
		}
		*stream++;
		break;
		LEX_CASE1('=', '=', TOKEN_EQ)
			LEX_CASE1('!', '=', TOKEN_NOTEQ)
			LEX_CASE1(':', '=', TOKEN_COLON_ASSIGN)
			LEX_CASE1('^', '=', TOKEN_XOR_ASSIGN)
			LEX_CASE1('*', '=', TOKEN_MULT_ASSIGN)
			LEX_CASE1('/', '=', TOKEN_DIV_ASSIGN)
			LEX_CASE1('%', '=', TOKEN_MOD_ASSIGN)
			LEX_CASE2('+', '+', TOKEN_INC, '=', TOKEN_ADD_ASSIGN)
			LEX_CASE2('-', '-', TOKEN_DEC, '=', TOKEN_MINUS_ASSIGN)
			LEX_CASE2('&', '&', TOKEN_AND, '=', TOKEN_AND_ASSIGN)
			LEX_CASE2('|', '|', TOKEN_OR, '=', TOKEN_OR_ASSIGN)

	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y':
	case '_': {
		while (isalnum(*stream) || *stream == '_') {
			stream++;
		}

		token.kind = TOKEN_NAME;
		token.name = str_intern_range(token.start, stream);
		break;
	}
	default:
		token.kind = *stream++;
		break;
	}

	token.end = stream;
}

#undef LEX_CASE1
#undef LEX_CASE2
#undef LEX_CASE3

void init_stream(const char* str) {
	stream = str;
	next_token();
}

void print_token(Token token) {
	switch (token.kind) {
	case TOKEN_INT:
		printf("TOKEN INT: %"PRIu64"\n", token.int_val);
		break;
	case TOKEN_FLOAT:
		printf("TOKEN FLOAT: %f\n", token.float_val);
		break;
	case TOKEN_NAME:
		printf("TOKEN NAME: %.*s\n", (int)(token.end - token.start), token.start);
		break;
	default:
		printf("TOKEN: %c\n", token.kind);
		break;
	}
}

inline bool is_token(TokenKind kind) {
	return token.kind == kind;
}

inline is_token_name(const char* name) {
	return is_token(TOKEN_NAME) && token.name == name;
}

inline bool match_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	return false;
}

inline bool expec_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	else {
		char buf[256];
		copy_token_kind_str(buf, sizeof(buf), kind);
		fatal("expected token %s, got %s", buf, token_kind_str(token.kind));
		return false;
	}
}

#define assert_token(x) assert(match_token(x));
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_string(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STRING))
#define assert_token_eof() assert(is_token(TOKEN_EOF))

void lex_test() {
	//Integer literal test
	init_stream("0 18446744073709551615 0xfffffffffffffff0 042 0b101");
	assert_token_int(0);
	assert_token_int(18446744073709551615ull);
	assert(token.mod == TOKENMOD_HEX);
	assert_token_int(0xfffffffffffffff0ull);
	assert(token.mod == TOKENMOD_OCT);
	assert_token_int(042);
	assert(token.mod == TOKENMOD_BIN);
	assert_token_int(0b101);
	assert_token_eof();

	//Float literal test
	init_stream("0.13 .123 42. 12e19 12e-19 12e+19");
	assert_token_float(0.13);
	assert_token_float(.123);
	assert_token_float(42.);
	assert_token_float(12e19);
	assert_token_float(12e-19);
	assert_token_float(12e+19);
	assert_token_eof();

	//Opratators test
	init_stream("< <= : + ++ - -- == += -= *= /= |= ^= &= != << >> ");
	assert_token('<');
	assert_token(TOKEN_LTEQ);
	assert_token(':');
	assert_token('+');
	assert_token(TOKEN_INC);
	assert_token('-');
	assert_token(TOKEN_DEC);
	assert_token(TOKEN_EQ);
	assert_token(TOKEN_ADD_ASSIGN);
	assert_token(TOKEN_MINUS_ASSIGN);
	assert_token(TOKEN_MULT_ASSIGN);
	assert_token(TOKEN_DIV_ASSIGN);
	assert_token(TOKEN_OR_ASSIGN);
	assert_token(TOKEN_XOR_ASSIGN);
	assert_token(TOKEN_AND_ASSIGN);
	assert_token(TOKEN_NOTEQ);
	assert_token(TOKEN_LSHIFT);
	assert_token(TOKEN_RSHIFT);
	assert_token_eof();

	//Char literal test
	init_stream(" 'f' '\\n' ");
	assert_token_int('f');
	assert_token_int('\n');
	assert_token_eof();

	//String literal test
	init_stream(" \"test\" \"l\\nb\"  ");
	assert_token_string(str_intern("test"));
	assert_token_string(str_intern("l\nb"));
	assert_token_eof();

	//Misc test.
	const char* str = "XY+(XY)_HELLO1,234+9941";
	init_stream(str);
	assert_token_name("XY");
	assert_token('+');
	assert_token('(');
	assert_token_name("XY");
	assert_token(')');
	assert_token_name("_HELLO1");
	assert_token(',');
	assert_token_int(234);
	assert_token('+');
	assert_token_int(9941);
	assert_token_eof();
}

#undef assert_token
#undef assert_token_eof
#undef assert_token_int
#undef assert_token_float
#undef assert_token_string