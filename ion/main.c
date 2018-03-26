#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>
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
#include "eval.c"
#include "resolve.c"

void main_test(void) {
    common_test();
    lex_test();
    // print_test();
    // parse_test();
    resolve_test();
}

int main(int argc, char **argv) {
    main_test();
    return 0;
}
