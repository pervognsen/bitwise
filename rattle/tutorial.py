from rattle import *

@module
class Xor:
    i1 = input(bit)
    i2 = input(bit)
    o = output((i1 & ~i2) | (~i1 & i2))

@module
class Not:
    i = input(bit)
    xor1 = Xor(i1=i, i2=1)
    o = output(xor1.o)

@module
class Example1:
    i1 = input(bit)
    i2 = input(bit)
    not1 = Not(i=i1)
    not2 = Not(i=i2)
    o = output(not1.o & not2.o)

import itertools
from itertools import product as cartesian_product

def linear_reduce(f, xs):
    xs = iter(xs)
    y = next(xs)
    for x in xs:
        y = f(y, x)
    return y

def binary_reduce(f, xs):
    def reduce(xs):
        if len(xs) == 1:
            return xs[0]
        else:
            i = len(xs) // 2
            return f(reduce(xs[:i]), reduce(xs[i:]))
    return reduce(list(xs))

#reduce = linear_reduce
reduce = binary_reduce

def reduce_or(xs):
    return reduce(lambda x, y: x | y, xs)

def reduce_and(xs):
    return reduce(lambda x, y: x & y, xs)

def reduce_xor(xs):
    return reduce(lambda x, y: x ^ y, xs)

def table_to_sum_of_products(table, inputs):
    return reduce_or(reduce_and(input if value else ~input for input, value in zip(inputs, values)) for values in table)

def tabulate(func, num_args=None):
    if num_args is None:
        num_args = func.__code__.co_argcount
    return {args for args in cartesian_product(*(num_args * [(0, 1)])) if func(*args)}

def function_to_sum_of_products(func, inputs):
    return table_to_sum_of_products(tabulate(func, len(inputs)), inputs)

@module
class Example2:
    i = input(bit[4])
    # Parity circuit: Classic example of exponential size blow-up when using sum-of-products representation
    o = output(function_to_sum_of_products(lambda x, y, z, w: x ^ y ^ z ^ w, i))

@module
class When:
    d0 = input(bit)
    d1 = input(bit)
    sel = input(bit)
    o = output((sel & d1) | (~sel & d0))

def function_to_muxes(func, inputs):
    if len(inputs) == 0:
        return func()
    else:
        first, rest = inputs[0], inputs[1:]
        when_true = function_to_muxes(lambda *args: func(1, *args), rest)
        when_false = function_to_muxes(lambda *args: func(0, *args), rest)
        return when(first, when_true, when_false)

@module
class Example3:
    i = input(bit[5])
    o = output(function_to_muxes(lambda *args: reduce_xor(args), i))

@module
class Example4:
    i = input(bit[8])
    o = output(reduce_xor(i))

@module
class Example5:
    i1 = input(bit[8])
    i2 = input(bit[8])
    # o = output(reduce_and(~(i1 ^ i2)))
    o = output(~reduce_or(i1 ^ i2))

def equals_constant(x, k):
    return reduce_and(bits(x[i] if k & (1 << i) else ~x[i] for i in range(len(x))))

@module
class Example6:
    i = input(bit[8])
    o = output(equals_constant(i, 5))

@module
class Add3:
    x = input(bit)
    y = input(bit)
    ci = input(bit)
    p = x ^ y
    g = x & y
    s = output(p ^ ci)
    co = output(g | (p & ci))
    # co = output((x & y) | (x & ci) | (y & ci))

def add3(x, y, c):
    adder = Add3(x=x, y=y, ci=c)
    return adder.s, adder.co

def adc(x, y, c=0):
    s = []
    for xi, yi in zip(x, y):
        si, c = add3(xi, yi, c)
        s.append(si)
    return bits(s), c

def add(x, y, c=0):
    s, c = adc(x, y, c)
    return s

N = 4

@module
class Example7:
    x = input(bit[N])
    y = input(bit[N])
    s = output(add(x, y))

def sub(x, y):
    return add(x, ~y, 1)

@module
class Example8:
    x = input(bit[N])
    y = input(bit[N])
    s = output(sub(x, y))

@module
class Example9:
    x = input(bit[N])
    y = input(bit[N])
    n = input(bit)
    s = output(add(x, when(n, ~y, y), n))

@module
class Example10:
    x = input(bit[N])
    y = input(bit[N])
    s, c = adc(x, ~y, 1)
    # greater or equal unsigned
    geu = output(c)
    # greater or equal signed
    ges = output(c ^ x[-1] ^ y[-1])

@module
class Example11:
    x = input(bit[N])
    y = input(bit[N])
    # Divide-and-conquer ripple-carry adder
    i = N//2
    s_lo, c_lo = adc(x[:i], y[:i], 0)
    s_hi, c_hi = adc(x[i:], y[i:], c_lo)
    s = output(s_lo @ s_hi)

@module
class Example12:
    # Carry-select adder
    x = input(bit[N])
    y = input(bit[N])
    i = N//2
    s_lo, c_lo = adc(x[:i], y[:i], 0)
    s_hi0, c_hi0 = adc(x[i:], y[i:], 0)
    s_hi1, c_hi1 = adc(x[i:], y[i:], 1)
    s = output(s_lo @ when(c_lo, s_hi1, s_hi0))

def conditional_sum_adder(x, y, c):
    assert len(x) == len(y)
    if len(x) == 1:
        s0, c = add3(x[0], y[0], c)
        return bits(s0), c
    else:
        i = len(x)//2
        s_lo, c_lo = conditional_sum_adder(x[:i], y[:i], c)
        s_hi0, c_hi0 = conditional_sum_adder(x[i:], y[i:], 0)
        s_hi1, c_hi1 = conditional_sum_adder(x[i:], y[i:], 1)
        return s_lo @ when(c_lo, s_hi1, s_hi0), when(c_lo, c_hi1, c_hi0)

@module
class Example13:
    x = input(bit[N])
    y = input(bit[N])
    s, c = conditional_sum_adder(x, y, 0)
    s = output(s)

def pg_compose(pg1, pg2):
    p1, g1 = pg1
    p2, g2 = pg2
    return p1 & p2, g2 | (p2 & g1)

def carry_lookahead_adder(x, y, scan):
    pg = [(xi | yi, xi & yi) for xi, yi in zip(x, y)]
    c = bits(g for p, g in scan(pg_compose, pg))
    return x ^ y ^ (c << 1)

def linear_scan(f, x):
    y = x[0]
    yield y
    for xi in x[1:]:
        y = f(y, xi)
        yield y

def naive_logarithmic_scan(f, x):
    for i in range(len(x)):
        yield binary_reduce(f, x[:i+1])

def sklansky_scan(f, x):
    if len(x) == 1:
        return [x[0]]
    else:
        i = len(x)//2
        y_lo, y_hi = sklansky_scan(f, x[:i]), sklansky_scan(f, x[i:])
        return y_lo + [f(y_lo[-1], yi) for yi in y_hi]

def interleave(xs, ys):
    assert len(xs) == len(ys)
    def helper(xs, ys):
        for x, y in zip(xs, ys):
            yield x
            yield y
    return list(helper(xs, ys))

def brent_kung_scan(f, x):
    if len(x) == 1:
        return [x[0]]
    else:
        y = brent_kung_scan(f, [f(x_even, x_odd) for x_even, x_odd in zip(x[::2], x[1::2])])
        return interleave([x[0]] + [f(y_odd, x_even) for x_even, y_odd in zip(x[2::2], y)], y)

def kogge_stone_scan(f, x):
    i = 1
    while i < len(x):
        x = x[:i] + [f(x0, x1) for x0, x1 in zip(x, x[i:])]
        i *= 2
    return x

@module
class Example14:
    i = input(bit[8])
    o = output(linear_scan(lambda x, y: x ^ y, i))

@module
class Example15:
    x = input(bit[N])
    y = input(bit[N])
    # scan = linear_scan
    # scan = naive_logarithmic_scan
    # scan = sklansky_scan
    # scan = brent_kung_scan
    scan = kogge_stone_scan
    s = output(carry_lookahead_adder(x, y, scan))

def make_carrylookahead_tester(scan):
    @module
    class Test:
        x = input(bit[N])
        y = input(bit[N])
        s = output(carry_lookahead_adder(x, y, scan))
    return Test

@module
class Mux4:
    sel = input(bit[2])
    i0 = input(bit[N])
    i1 = input(bit[N])
    i2 = input(bit[N])
    i3 = input(bit[N])
    o = output(when(sel[0], when(sel[1], i3, i1), when(sel[1], i2, i0)))

def mux4(sel, i0, i1, i2, i3):
    inst = Mux4(sel=sel, i0=i0, i1=i1, i2=i2, i3=i3)
    return inst.o

def left_shifter_radix2(x, n):
    assert 2**len(n) == len(x)
    for i, b in enumerate(n):
        x = when(b, x << 2**i, x)
    return x

def left_shifter_radix4(x, n):
    assert 2**len(n) == len(x)
    for i in range(0, len(n), 2):
        if i + 2 <= len(n):
            x = mux4(n[i:i+2], x, x << 2**i, x << 2**(i+1), x << (2**i + 2**(i+1)))
        else:
            x = when(n[i], x << 2**i, x)
    return x

def left_rotator_radix2(x, n):
    assert 2**len(n) == len(x)
    for i, b in enumerate(n):
        x = when(b, x[-2**i:] @ x[:-2**i], x)
    return x

def left_rotator_radix4(x, n):
    assert 2**len(n) == len(x)
    for i in range(0, len(n), 2):
        m = 2**i
        if i+2 <= len(n):
            x = mux4(n[i:i+2], x, x[-m:] @ x[:-m], x[-2*m:] @ x[:-2*m], x[-3*m:] @ x[:-3*m])
        else:
            x = when(n[i], x[-2**i:] @ x[:-2**i], x)
    return x

def right_rotator_radix2(x, n):
    # return left_rotator_radix2(x, -n)
    return left_rotator_radix2(x[-1] @ x[:-1], ~n)

def barrel_left_shifter(x, n):
    mask = bits(i >= n for i in range(len(x)))
    return left_rotator_radix2(x, n) & mask

def barrel_logical_right_shifter(x, n):
    mask = bits(i + n @ 0 < len(x) for i in range(len(x)))
    return right_rotator_radix2(x, n) & mask

def barrel_arithmetic_right_shifter(x, n):
    y = right_rotator_radix2(x, n)
    return bits(when(i + n @ 0 < len(x), b, x[-1]) for i, b in enumerate(y))

def barrel_shifter(x, n, dir, shift, arith, left_rotator=left_rotator_radix4):
    y_rotate = left_rotator(when(dir, x[-1] @ x[:-1], x), when(dir, ~n, n))
    mask = when(shift, bits(when(dir, i + n @ 0 < len(x), i >= n) for i in range(len(x))), ~0)
    return bits(when(mask[i], b, arith & x[-1]) for i, b in enumerate(y_rotate))

@module
class Example16:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(left_shifter_radix2(x, n))

@module
class Example17:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(left_shifter_radix4(x, n))

@module
class Example18:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(left_rotator_radix4(x, n))

@module
class Example19:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(right_rotator_radix2(x, n))

@module
class Example20:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(barrel_left_shifter(x, n))

@module
class Example21:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(barrel_logical_right_shifter(x, n))

@module
class Example22:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(barrel_arithmetic_right_shifter(x, n))

@module
class Example23:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    dir = input(bit)
    shift = input(bit)
    arith = input(bit)
    y = output(barrel_shifter(x, n, dir, shift, arith))

def funnel_shifter(x, n):
    assert 2 * 2**len(n) - 1 == len(x)
    for i, b in enumerate(n):
        x = when(b, x[2**i:], x[:-2**i])
    return x

def funnel_right_shifter(x, n):
    return funnel_shifter(x @ bit[len(x)-1](0), n)

def funnel_arithmetic_right_shifter(x, n):
    return funnel_shifter(x @ rep(x[-1], len(x)-1), n)

def funnel_left_shifter(x, n):
    return funnel_shifter(bit[len(x)-1](0) @ x, ~n)

def funnel_right_rotator(x, n):
    return funnel_shifter(x @ x[:-1], n)

def funnel_left_rotator(x, n):
    return funnel_shifter(x[1:] @ x, ~n)

@module
class Example24:
    x = input(bit[2*N - 1])
    n = input(bit[clog2(N-1)])
    y = output(funnel_shifter(x, n))

@module
class Example25:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(funnel_right_shifter(x, n))

@module
class Example26:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(funnel_left_shifter(x, n))

@module
class Example27:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(funnel_arithmetic_right_shifter(x, n))

@module
class Example28:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(funnel_right_rotator(x, n))

@module
class Example29:
    x = input(bit[N])
    n = input(bit[clog2(N-1)])
    y = output(funnel_left_rotator(x, n))

open('example.dot', 'w').write(generate_dot_file(Example24))

do_timing_analysis = False
if do_timing_analysis:
    print("Ripple-carry:", analyze_delay(Example7))
    print("Recursive ripple-carry:", analyze_delay(Example11))
    print("Carry-select:", analyze_delay(Example12))
    print("Conditional sum:", analyze_delay(Example13))

    for scan in (linear_scan, naive_logarithmic_scan, sklansky_scan, brent_kung_scan, kogge_stone_scan):
        print("Carry-lookahead with %s:" % scan.__name__, analyze_delay(make_carrylookahead_tester(scan)))

print(analyze_delay(Example23))

mask = (1 << N) - 1

uints = range(2**N)
sints = range(-2**(N-1), 2**(N-1))
shifts = range(N)
fints = range(2**(2*N - 1))

def rotl(x, n):
    return ((x << n) & mask) | ((x >> (N - n)) & mask)

def rotr(x, n):
    return ((x >> n) & mask) | ((x << (N - n)) & mask)

do_tests = True
if do_tests:
    # example7 = compile(Example7)
    # for x in uints:
    #     for y in uints:
    #         assert (x + y) & mask == example7.evaluate(x, y).s

    # example8 = compile(Example8)
    # for x in uints:
    #     for y in uints:
    #         assert (x - y) & mask == example8.evaluate(x, y).s

    # example10 = compile(Example10)
    # for x in uints:
    #     for y in uints:
    #         assert (x >= y) == example10.evaluate(x, y).geu

    # for x in sints:
    #     for y in sints:
    #         assert (x >= y) == example10.evaluate(x, y).ges

    # example11 = compile(Example11)
    # for x in uints:
    #     for y in uints:
    #         s = example11.evaluate(x, y).s
    #         assert (x + y) & mask == s

    # example12 = compile(Example12)
    # for x in uints:
    #     for y in uints:
    #         s = example12.evaluate(x, y).s
    #         assert (x + y) & mask == s
    
    # example13 = compile(Example13)
    # for x in uints:
    #     for y in uints:
    #         s = example13.evaluate(x, y).s
    #         assert (x + y) & mask == s

    # example15 = compile(Example15, trace=False)
    # for x in uints:
    #     for y in uints:
    #         trace = x == 3 and y == 8
    #         s = example15.evaluate(x, y, trace=trace).s
    #         assert (x + y) & mask == s

    # example16 = compile(Example16)
    # for x in uints:
    #     for n in shifts:
    #         y = example16.evaluate(x, n).y
    #         assert (x << n) & mask == y

    # example17 = compile(Example17)
    # for x in uints:
    #     for n in shifts:
    #         y = example17.evaluate(x, n).y
    #         assert (x << n) & mask == y

    # example18 = compile(Example18)
    # for x in uints:
    #     for n in shifts:
    #         y = example18.evaluate(x, n).y
    #         assert rotl(x, n) == y

    # example19 = compile(Example19)
    # for x in uints:
    #     for n in shifts:
    #         y = example19.evaluate(x, n).y
    #         assert rotr(x, n) == y

    # example20 = compile(Example20)
    # for x in uints:
    #     for n in shifts:
    #         y = example20.evaluate(x, n).y
    #         assert (x << n) & mask == y

    # example21 = compile(Example21)
    # for x in uints:
    #     for n in shifts:
    #         y = example21.evaluate(x, n).y
    #         assert (x >> n) & mask == y

    # example22 = compile(Example22)
    # for x in sints:
    #     for n in shifts:
    #         y = example22.evaluate(x, n).y
    #         assert (x >> n) & mask == y

    # example23 = compile(Example23)
    # for x in uints:
    #     for n in shifts:
    #         y = example23.evaluate(x, n, dir=0, shift=0, arith=0).y
    #         assert rotl(x, n) == y
    # for x in uints:
    #     for n in shifts:
    #         y = example23.evaluate(x, n, dir=1, shift=0, arith=0).y
    #         assert rotr(x, n) == y
    # for x in uints:
    #     for n in shifts:
    #         y = example23.evaluate(x, n, dir=0, shift=1, arith=0).y
    #         assert (x << n) & mask == y
    # for x in uints:
    #     for n in shifts:
    #         y = example23.evaluate(x, n, dir=1, shift=1, arith=0).y
    #         assert (x >> n) & mask == y
    # for x in sints:
    #     for n in shifts:
    #         y = example23.evaluate(x, n, dir=1, shift=1, arith=1).y
    #         assert (x >> n) & mask == y

    example24 = compile(Example24)
    for x in fints:
        for n in shifts:
            y = example24.evaluate(x, n).y
            assert (x >> n) & mask == y

    example25 = compile(Example25)
    for x in uints:
        for n in shifts:
            y = example25.evaluate(x, n).y
            assert (x >> n) & mask == y

    example26 = compile(Example26)
    for x in uints:
        for n in shifts:
            y = example26.evaluate(x, n).y
            assert (x << n) & mask == y

    example27 = compile(Example27)
    for x in sints:
        for n in shifts:
            y = example27.evaluate(x, n).y
            assert (x >> n) & mask == y

    example28 = compile(Example28)
    for x in uints:
        for n in shifts:
            y = example28.evaluate(x, n).y
            assert rotr(x, n) == y

    example29 = compile(Example29)
    for x in uints:
        for n in shifts:
            y = example29.evaluate(x, n).y
            assert rotl(x, n) == y
