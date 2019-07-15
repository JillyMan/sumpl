void *xrealloc(void* buffer, size_t num_bytes) {
	void *ptr = realloc(buffer, num_bytes);
	if (!ptr) {
		perror("xrealloc failed");
		exit(1);
	}
	return ptr;
}

void *xcalloc(size_t count, size_t size) {
	void* ptr = calloc(count, size);
	if (!ptr) {
		perror("xcalloc failed");
		exit(1);
	}
	return ptr;
}

void *xmalloc(size_t num_bytes) {
	void *ptr = malloc(num_bytes);
	if (!ptr) {
		perror("xmalloc failed");
		exit(1);
	}
	return ptr;
}

void fatal(const char *format, ...) {
	va_list args; 
	va_start(args, format);
	printf("FATAL: ");
	vprintf(format, args);
	printf("\n");
	va_end(args);
	exit(1);
}

void syntax_error(const char* format, ...) {
	va_list args;
	va_start(args, format);
	printf("Syntax Error: ");
	vprintf(format, args);
	printf("\n");
	va_end(args);
}

typedef struct BufHdr {
	size_t len;
	size_t cap;
	char buf[0];
} BufHdr;

#define line() printf("-------------------------------\n");

#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define MAX(x, y) ((x) >= (y) ? (x) : (y))

#define BUF(x) x
#define buf__hdr(b) ((BufHdr *)((char *)(b) - offsetof(BufHdr, buf)))

#define buf__fits(b, n) ((buf_len(b) + (n)) <= buf_cap(b))
#define buf_reserve(b, n) (buf__fits((b), (n)) ? 0 : ((b) = buf__grow((b), buf_len(b) + (n), sizeof(*(b)))))
//BUF__FIT

#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_end(b) ((b) + buf_len(b))
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : 0)
#define buf_push(b, x) (buf_reserve((b), (1)), b[buf__hdr(b)->len++] = (x))

void *buf__grow(const void *buf, size_t new_len, size_t elem_size) {
	size_t new_cap = MAX(1 + 2 * buf_cap(buf), new_len);
	assert(new_len <= new_cap);
	size_t new_size = offsetof(BufHdr, buf) + new_cap * elem_size;
	BufHdr* new_hdr;
	if (buf ) {
		new_hdr = xrealloc(buf__hdr(buf), new_size);
	}
	else {
		new_hdr = xmalloc(new_size);
		new_hdr->len = 0;
	}
	new_hdr->cap = new_cap;
	return new_hdr->buf;
}

void buf_test() {
	int *buf = NULL;
	int N = 10;
	for (int i = 0; i < N; ++i) {
		buf_push(buf, i);
	}
	assert(buf_len(buf) == N);
	for (int i = 0; i < N; ++i) {
		buf[i];
	}

	buf_free(buf);
	assert(buf == NULL);
	assert(buf_len(buf) == 0);
	line();
}

typedef struct Intern {
	size_t len;
	const char* str;
} Intern;

static Intern* interns;

const char *str_intern_range(const char *start, const char *end) {
	size_t len = end - start;

	for(Intern* it = interns; it != buf_end(interns); ++it) {
		if (it->len == len && strncmp(it->str, start, len) == 0) {
			return it->str;
		}
	}

	char *str = xmalloc(len + 1);
	memcpy(str, start, len);
	str[len] = 0;
	buf_push(interns, ((Intern){ len, str }));
	return str;
}

const char* str_intern(const char* str) {
	return str_intern_range(str, str + strlen(str));
}

void str_intern_test() {
	char x[] = "test";
	char y[] = "test";
	char z[] = "other test";
	assert(x != y);

	const char *px = str_intern(x);
	const char *py = str_intern(y);
	const char *pz = str_intern(z);
	assert(px == py);

	for (int i = 0; i < buf_len(interns); ++i) {
		printf("interns[%d]: %s\n", i, interns[i].str);
	}
}


void common_tests() {
	buf_test();
	str_intern_test();
}