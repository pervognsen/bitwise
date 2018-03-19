#include "common.c"
#include "lex.c"

void run_tests() {
    common_test();
    lex_test();
}

int main(int argc, char **argv) {
    run_tests();
    return 0;
}

