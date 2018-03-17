#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#define MAX(x, y) ((x) >= (y) ? (x) : (y))

void *xrealloc(void *ptr, size_t num_bytes) {
    ptr = realloc(ptr, num_bytes);
    if (!ptr) {
        perror("xrealloc failed");
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

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

// Stretchy buffers, invented (?) by Sean Barrett

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[];
} BufHdr;

#define buf__hdr(b) ((BufHdr *)((char *)(b) - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n) (buf__fits((b), (n)) ? 0 : ((b) = buf__grow((b), buf_len(b) + (n), sizeof(*(b)))))

#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_end(b) ((b) + buf_len(b))
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : 0)
#define buf_push(b, ...) (buf__fit((b), 1), (b)[buf__hdr(b)->len++] = (__VA_ARGS__))

void *buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    assert(buf_cap(buf) <= (SIZE_MAX - 1)/2);
    size_t new_cap = MAX(1 + 2*buf_cap(buf), new_len);
    assert(new_len <= new_cap);
    assert(new_cap <= (SIZE_MAX - offsetof(BufHdr, buf))/elem_size);
    size_t new_size = offsetof(BufHdr, buf) + new_cap*elem_size;
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = xrealloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = xmalloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;
    return new_hdr->buf;
}

void buf_test() {
    int *buf = NULL;
    assert(buf_len(buf) == 0);
    int n = 1024;
    for (int i = 0; i < n; i++) {
        buf_push(buf, i);
    }
    assert(buf_len(buf) == n);
    for (int i = 0; i < buf_len(buf); i++) {
        assert(buf[i] == i);
    }
    buf_free(buf);
    assert(buf == NULL);
    assert(buf_len(buf) == 0);
}

typedef struct Intern {
    size_t len;
    const char *str;
} Intern;

static Intern *interns;

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    for (Intern *it = interns; it != buf_end(interns); it++) {
        if (it->len == len && strncmp(it->str, start, len) == 0) {
            return it->str;
        }
    }
    char *str = xmalloc(len + 1);
    memcpy(str, start, len);
    str[len] = 0;
    buf_push(interns, (Intern){len, str});
    return str;
}

const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

void str_intern_test() {
    char a[] = "hello";
    assert(strcmp(a, str_intern(a)) == 0);
    assert(str_intern(a) == str_intern(a));
    assert(str_intern(str_intern(a)) == str_intern(a));
    char b[] = "hello";
    assert(a != b);
    assert(str_intern(a) == str_intern(b));
    char c[] = "hello!";
    assert(str_intern(a) != str_intern(c));
    char d[] = "hell";
    assert(str_intern(a) != str_intern(d));
}

typedef enum TokenKind {
    // Reserve first 128 values for one-char tokens
    TOKEN_LAST_CHAR = 127,
    TOKEN_INT,
    TOKEN_NAME,
    // ...
} TokenKind;

size_t copy_token_kind_str(char *dest, size_t dest_size, TokenKind kind) {
    size_t n = 0;
    switch (kind) {
    case 0:
        n = snprintf(dest, dest_size, "end of file");
        break;
    case TOKEN_INT:
        n = snprintf(dest, dest_size, "integer");
        break;
    case TOKEN_NAME:
        n = snprintf(dest, dest_size, "name");
        break;
    default:
        if (kind < 128 && isprint(kind)) {
            n = snprintf(dest, dest_size, "%c", kind);
        } else {
            n = snprintf(dest, dest_size, "<ASCII %d>", kind);
        }
        break;
    }
    return n;
}

// Warning: This returns a pointer to a static internal buffer, so the next call will overwrite it.
const char *temp_token_kind_str(TokenKind kind) {
    static char buf[256];
    size_t n = copy_token_kind_str(buf, sizeof(buf), kind);
    assert(n + 1 <= sizeof(buf));
    return buf;
}

typedef struct Token {
    TokenKind kind;
    const char *start;
    const char *end;
    union {
        int val;
        const char *name;
    };
} Token;

Token token;
const char *stream;

const char *keyword_if;
const char *keyword_for;
const char *keyword_while;

void init_keywords() {
    keyword_if = str_intern("if");
    keyword_for = str_intern("for");
    keyword_while = str_intern("while");
    // ...
}

void next_token() {
    token.start = stream;
    switch (*stream) {
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
        int val = 0;
        while (isdigit(*stream)) {
            val *= 10;
            val += *stream++ - '0';
        }
        token.kind = TOKEN_INT;
        token.val = val;
        break;
    }
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case '_':
        while (isalnum(*stream) || *stream == '_') {
            stream++;
        }
        token.kind = TOKEN_NAME;
        token.name = str_intern_range(token.start, stream);
        break;
    default:
        token.kind = *stream++;
        break;
    }
    token.end = stream;
}

void init_stream(const char *str) {
    stream = str;
    next_token();
}

void print_token(Token token) {
    switch (token.kind) {
    case TOKEN_INT:
        printf("TOKEN INT: %d\n", token.val);
        break;
    case TOKEN_NAME:
        printf("TOKEN NAME: %.*s\n", (int)(token.end - token.start), token.start);
        break;
    default:
        printf("TOKEN '%c'\n", token.kind);
        break;
    }
}

static inline bool is_token(TokenKind kind) {
    return token.kind == kind;
}

static inline bool is_token_name(const char *name) {
    return token.kind == TOKEN_NAME && token.name == name;
}

static inline bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

static inline bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        char buf[256];
        copy_token_kind_str(buf, sizeof(buf), kind);
        fatal("expected token %s, got %s", buf, temp_token_kind_str(token.kind));
        return false;
    }
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.val == (x) && match_token(TOKEN_INT))
#define assert_token_eof() assert(is_token(0))

void lex_test() {
    const char *str = "XY+(XY)_HELLO1,234+994";
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
    assert_token_int(994);
    assert_token_eof();
}

#undef assert_token_eof
#undef assert_token_int
#undef assert_token_name
#undef assert_token

#if 0
    expr3 = INT | '(' expr ')' 
    expr2 = '-' expr2 | expr3
    expr1 = expr2 ([*/] expr2)*
    expr0 = expr1 ([+-] expr1)*
    expr = expr0
#endif

int parse_expr();

int parse_expr3() {
    if (is_token(TOKEN_INT)) {
        int val = token.val;
        next_token();
        return val;
    } else if (match_token('(')) {
        int val = parse_expr();
        expect_token(')');
        return val;
    } else {
        fatal("expected integer or (, got %s", temp_token_kind_str(token.kind));
        return 0;
    }
}

int parse_expr2() {
    if (match_token('-')) {
        return -parse_expr2();
    } else if (match_token('+')) {
        return parse_expr2();
    } else {
        return parse_expr3();
    }
}

int parse_expr1() {
    int val = parse_expr2();
    while (is_token('*') || is_token('/')) {
        char op = token.kind;
        next_token();
        int rval = parse_expr2();
        if (op == '*') {
            val *= rval;
        } else {
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
        } else {
            assert(op == '-');
            val -= rval;
        }
    }
    return val;
}

int parse_expr() {
    return parse_expr0();
}

int parse_expr_str(const char *str) {
    init_stream(str);
    return parse_expr();
}

#define assert_expr(x) assert(parse_expr_str(#x) == (x))

void parse_test() {
    assert_expr(1);
    assert_expr((1));
    assert_expr(-+1);
    assert_expr(1-2-3);
    assert_expr(2*3+4*5);
    assert_expr(2*(3+4)*5);
    assert_expr(2+-3);
}

#undef assert_expr

void run_tests() {
    buf_test();
    lex_test();
    str_intern_test();
    parse_test();
}

int main(int argc, char **argv) {
    run_tests();
    return 0;
}

