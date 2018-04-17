template = """
func example_test(?)(): int {
    return fact_rec(?)(10) == fact_iter(?)(10);
}

union IntOrPtr(?) {
    i: int;
    p: int*;
}

// func f(?)() {
//    u1 := IntOrPtr(?){i = 42};
//    u2 := IntOrPtr(?){p = (:int*)42};
//    u1.i = 0;
//    u2.p = (:int*)0;
// }

var i(?): int;

struct Vector(?) {
    x, y: int;
}

func fact_iter(?)(n: int): int {
    r := 1;
    for (i := 2; i <= n; i++) {
        r *= i;
    }
    return r;
}

func fact_rec(?)(n: int): int {
    if (n == 0) {
        return 1;
    } else {
        return n * fact_rec(?)(n-1);
    }
}

const n(?) = 1 + sizeof(p(?));

var p(?): T(?)*;

struct T(?) {
    a: int[n(?)];
}
"""

print("func main(argc: int, argv: char**): int { return 0; }")

for i in range(128 * 1024):
    print(template.replace("(?)", str(i)))
