typedef struct SrcLoc {
    const char *name;
    int line;
} SrcLoc;

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

extern int src_line;
extern const char *src_name;

void error(SrcLoc loc, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("%s(%d): ", loc.name, loc.line);
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

#define fatal_error(...) (error(__VA_ARGS__), exit(1))
#define syntax_error(...) (error((SrcLoc){src_name, src_line}, __VA_ARGS__))
#define fatal_syntax_error(...) (syntax_error(__VA_ARGS__), exit(1))
