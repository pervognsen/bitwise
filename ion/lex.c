typedef enum TokenKind {
    TOKEN_EOF = 0,
    // Reserve first 128 values for one-char tokens
    TOKEN_LAST_CHAR = 127,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_STR,
    TOKEN_NAME,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
} TokenKind;

typedef enum TokenMod {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_BIN,
    TOKENMOD_OCT,
    TOKENMOD_CHAR,
} TokenMod;

size_t copy_token_kind_str(char *dest, size_t dest_size, TokenKind kind) {
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
    TokenMod mod;
    const char *start;
    const char *end;
    union {
        uint64_t int_val;
        double float_val;
        const char *str_val;
        const char *name;
    };
} Token;

Token token;
const char *stream;

uint8_t char_to_digit[256] = {
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
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

void scan_int() {
    uint64_t base = 10;
    if (*stream == '0') {
        stream++;
        if (tolower(*stream) == 'x') {
            stream++;
            token.mod = TOKENMOD_HEX;
            base = 16;
        } else if (tolower(*stream) == 'b') {
            stream++;
            token.mod = TOKENMOD_BIN;
            base = 2;
        } else if (isdigit(*stream)) {
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
        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);
            digit = 0;
        }
        if (val > (UINT64_MAX - digit)/base) {
            syntax_error("Integer literal overflow");
            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
            break;
        }
        val = val*base + digit;
        stream++;
    }
    token.kind = TOKEN_INT;
    token.int_val = val;
}

void scan_float() {
    const char *start = stream;
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
            syntax_error("Expected digit after float literal exponent, found '%c'.", *stream);
        }
        while (isdigit(*stream)) {
            stream++;
        }
    }
    const char *end = stream;
    double val = strtod(start, NULL);
    if (val == HUGE_VAL || val == -HUGE_VAL) {
        syntax_error("Float literal overflow");
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
        syntax_error("Char literal cannot be empty");
        stream++;
    } else if (*stream == '\n') {
        syntax_error("Char literal cannot contain newline");
    } else if (*stream == '\\') {
        stream++;
        val = escape_to_char[*stream];
        if (val == 0 && *stream != '0') {
            syntax_error("Invalid char literal escape '\\%c'", *stream);
        }
        stream++;
    } else {
        val = *stream;
        stream++;
    }
    if (*stream != '\'') {
        syntax_error("Expected closing char quote, got '%c'", *stream);
    } else {
        stream++;
    }
    token.kind = TOKEN_INT;
    token.int_val = val;
    token.mod = TOKENMOD_CHAR;
}

void scan_str() {
    assert(*stream == '"');
    stream++;
    char *str = NULL;
    while (*stream && *stream != '"') {
        char val = *stream;
        if (val == '\n') {
            syntax_error("String literal cannot contain newline");
            break;
        } else if (val == '\\') {
            stream++;
            val = escape_to_char[*stream];
            if (val == 0 && *stream != '0') {
                syntax_error("Invalid string literal escape '\\%c'", *stream);
            }
        }
        buf_push(str, val);
        stream++;
    }
    if (*stream) {
        assert(*stream == '"');
        stream++;
    } else {
        syntax_error("Unexpected end of file within string literal");
    }
    buf_push(str, 0);
    token.kind = TOKEN_STR;
    token.str_val = str;
}

#define CASE1(c, c1, k1) \
    case c: \
        token.kind = *stream++; \
        if (*stream == c1) { \
            token.kind = k1; \
            stream++; \
        } \
        break;

#define CASE2(c, c1, k1, c2, k2) \
    case c: \
        token.kind = *stream++; \
        if (*stream == c1) { \
            token.kind = k1; \
            stream++; \
        } else if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } \
        break;

void next_token() {
repeat:
    token.start = stream;
    token.mod = 0;
    switch (*stream) {
    case ' ': case '\n': case '\r': case '\t': case '\v':
        while (isspace(*stream)) {
            stream++;
        }
        goto repeat;
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
        while (isdigit(*stream)) {
            stream++;
        }
        char c = *stream;
        stream = token.start;
        if (c == '.' || tolower(c) == 'e') {
            scan_float();
        } else {
            scan_int();
        }
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
    case '<':
        token.kind = *stream++;
        if (*stream == '<') {
            token.kind = TOKEN_LSHIFT;
            stream++;
            if (*stream == '=') {
                token.kind = TOKEN_LSHIFT_ASSIGN;
                stream++;
            }
        } else if (*stream == '=') {
            token.kind = TOKEN_LTEQ;
            stream++;
        }
        break;
    case '>':
        token.kind = *stream++;
        if (*stream == '>') {
            token.kind = TOKEN_RSHIFT;
            stream++;
            if (*stream == '=') {
                token.kind = TOKEN_RSHIFT_ASSIGN;
                stream++;
            }
        } else if (*stream == '=') {
            token.kind = TOKEN_GTEQ;
            stream++;
        }
        break;
    CASE1('^', '=', TOKEN_XOR_ASSIGN)
    CASE1(':', '=', TOKEN_COLON_ASSIGN)
    CASE1('*', '=', TOKEN_MUL_ASSIGN)
    CASE1('/', '=', TOKEN_DIV_ASSIGN)
    CASE1('%', '=', TOKEN_MOD_ASSIGN)
    CASE2('+', '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC)
    CASE2('-', '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC)
    CASE2('&', '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND)
    CASE2('|', '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR)
    default:
        token.kind = *stream++;
        break;
    }
    token.end = stream;
}

#undef CASE1
#undef CASE2

void init_stream(const char *str) {
    stream = str;
    next_token();
}

void print_token(Token token) {
    switch (token.kind) {
    case TOKEN_INT:
        printf("TOKEN INT: %" PRIu64 "\n", token.int_val);
        break;
    case TOKEN_FLOAT:
        printf("TOKEN FLOAT: %f\n", token.float_val);
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
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

void lex_test() {
    // Integer literal tests
    init_stream("0 18446744073709551615 0xffffffffffffffff 042 0b1111");
    assert_token_int(0);
    assert_token_int(18446744073709551615ull);
    assert(token.mod == TOKENMOD_HEX);
    assert_token_int(0xffffffffffffffffull);
    assert(token.mod == TOKENMOD_OCT);
    assert_token_int(042);
    assert(token.mod == TOKENMOD_BIN);
    assert_token_int(0xF);
    assert_token_eof();

    // Float literal tests
    init_stream("3.14 .123 42. 3e10");
    assert_token_float(3.14);
    assert_token_float(.123);
    assert_token_float(42.);
    assert_token_float(3e10);
    assert_token_eof();

    // Char literal tests
    init_stream("'a' '\\n'");
    assert_token_int('a');
    assert_token_int('\n');
    assert_token_eof();

    // String literal tests
    init_stream("\"foo\" \"a\\nb\"");
    assert_token_str("foo");
    assert_token_str("a\nb");
    assert_token_eof();

    // Operator tests
    init_stream(": := + += ++ < <= << <<=");
    assert_token(':');
    assert_token(TOKEN_COLON_ASSIGN);
    assert_token('+');
    assert_token(TOKEN_ADD_ASSIGN);
    assert_token(TOKEN_INC);
    assert_token('<');
    assert_token(TOKEN_LTEQ);
    assert_token(TOKEN_LSHIFT);
    assert_token(TOKEN_LSHIFT_ASSIGN);
    assert_token_eof();

    // Misc tests
    init_stream("XY+(XY)_HELLO1,234+994");
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

