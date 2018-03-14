#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

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
        perror("malloc failed");
        exit(1);
    }
    return ptr;
}

// stretchy buffers, invented (?) by sean barrett

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[0];
} BufHdr;

#define buf__hdr(b) ((BufHdr *)((char *)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n) (buf__fits(b, n) ? 0 : ((b) = buf__grow((b), buf_len(b) + (n), sizeof(*(b)))))

#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_push(b, x) (buf__fit(b, 1), b[buf_len(b)] = (x), buf__hdr(b)->len++)
#define buf_free(b) ((b) ? free(buf__hdr(b)) : 0, (b) = NULL)

void *buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    size_t new_cap = MAX(1 + 2*buf_cap(buf), new_len);
    assert(new_len <= new_cap);
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
    int *asdf = NULL;
    enum { N = 1024 };
    for (int i = 0; i < N; i++) {
        buf_push(asdf, i);
    }
    assert(buf_len(asdf) == N);
    for (int i = 0; i < buf_len(asdf); i++) {
        assert(asdf[i] == i);
    }
    buf_free(asdf);
}

// lexing: translating char stream to token stream

typedef enum TokenKind {
    TOKEN_INT = 128,
    TOKEN_NAME,
    // ...
} TokenKind;

typedef struct Token {
    TokenKind kind;
    union {
        uint64_t val;
        struct {
            const char *start;
            const char *end;
        };
    };
} Token;

Token token;
const char *stream;

void next_token() {
    switch (*stream) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        uint64_t val = 0;
        while (isdigit(*stream)) {
            val *= 10;
            val += *stream++ - '0';
        }
        token.kind = TOKEN_INT;
        token.val = val;
        break;
    }
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case '_': {
        const char *start = stream++;
        while (isalnum(*stream) || *stream == '_') {
            stream++;
        }
        token.kind = TOKEN_NAME;
        token.start = start;
        token.end = stream;
        break;
    }
    default:
        token.kind = *stream++;
        break;
    }    
}

void print_token(Token token) {
    switch (token.kind) {
    case TOKEN_INT:
        printf("TOKEN INT: %llu\n", token.val);
        break;
    case TOKEN_NAME:
        printf("TOKEN NAME: %.*s\n", (int)(token.end - token.start), token.start);
        break;
    default:
        printf("TOKEN '%c'\n", token.kind);
        break;
    }
}

void lex_test() {
    char *source = "+()_HELLO1,234+FOO!994";
    stream = source;
    next_token();
    while (token.kind) {
        print_token(token);
        next_token();
    }
}

int main(int argc, char **argv) {
    buf_test();
    lex_test();
    return 0;
}

