#if _MSC_VER >= 1900 || __STDC_VERSION__ >= 201112L
// Visual Studio 2015 supports enough C99/C11 features for us.
#else
#error "C11 support required or Visual Studio 2015 or later"
#endif

#define _CRT_SECURE_NO_WARNINGS
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>
#include <limits.h>
#include <assert.h>
#include <stdlib.h>

bool flag_verbose;
bool flag_lazy;
bool flag_notypeinfo;
bool flag_fullgen;

#include "common.c"
#include "os.c"
#include "lex.c"
#include "type.c"
#include "ast.h"
#include "ast.c"
#include "print.c"
#include "parse.c"
#include "targets.c"
#include "resolve.c"
#include "gen.c"
#include "ion.c"
#include "test.c"

int main(int argc, const char **argv) {
//    main_test();
    return ion_main(argc, argv);
}
