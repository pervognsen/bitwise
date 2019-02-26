#include "stdafx.h"

bool flag_verbose;
bool flag_lazy;
bool flag_notypeinfo;
bool flag_fullgen;
bool flag_nolinesync;

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
    return ion_main(argc, argv);
}
