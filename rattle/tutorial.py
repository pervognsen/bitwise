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

def logarithmic_reduce(f, xs):
    def reduce(xs):
        if len(xs) == 1:
            return xs[0]
        else:
            i = len(xs) // 2
            return f(reduce(xs[:i]), reduce(xs[i:]))
    return reduce(list(xs))

#reduce = linear_reduce
reduce = logarithmic_reduce

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
    # less than unsigned
    ltu = output(~c)
    # less than signed
    lts = output(c ^ x[-1] ^ ~y[-1])

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
        assert i*2 == len(x)
        s_lo, c_lo = conditional_sum_adder(x[:i], y[:i], c)
        s_hi0, c_hi0 = conditional_sum_adder(x[i:], y[i:], 0)
        s_hi1, c_hi1 = conditional_sum_adder(x[i:], y[i:], 1)
        s = s_lo @ when(c_lo, s_hi1, s_hi0)
        assert len(s) == len(s_lo) + len(s_hi0)
        assert len(s) == len(x)
        c = when(c_lo, c_hi1, c_hi0)
        assert c.type == bit
        return s, c

@module
class Example13:
    x = input(bit[N])
    y = input(bit[N])
    s, c = conditional_sum_adder(x, y, 0)
    s = output(s)

@module
class PG:
    p1, g1 = input(bit), input(bit)
    p2, g2 = input(bit), input(bit)
    p = output(p1 & p2)
    g = output(g2 | (p2 & g1))

def pg_compose(pg1, pg2):
    p1, g1 = pg1
    p2, g2 = pg2
    pg = PG(p1=p1, g1=g1, p2=p2, g2=g2)
    return pg.p, pg.g

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

def bruteforce_logarithmic_scan(f, x):
    for i in range(len(x)):
        yield logarithmic_reduce(f, x[:i+1])

def sklansky_scan(f, x):
    if len(x) == 1:
        return [x[0]]
    else:
        i = len(x)//2
        y_lo = sklansky_scan(f, x[:i])
        y_hi = sklansky_scan(f, x[i:])
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

@module
class Example14:
    i = input(bit[8])
    o = output(linear_scan(lambda x, y: x ^ y, i))

@module
class Example15:
    x = input(bit[N])
    y = input(bit[N])
    # scan = linear_scan
    # scan = bruteforce_logarithmic_scan
    # scan = sklansky_scan
    scan = brent_kung_scan
    s = output(carry_lookahead_adder(x, y, scan))

open('example.dot', 'w').write(generate_dot_file(Example15))

mask = (1 << N) - 1

uints = range(2**N)
sints = range(-2**(N-1), 2**(N-1))

run_tests = True
if run_tests:
    example7 = compile(Example7)
    for x in uints:
        for y in uints:
            assert (x + y) & mask == example7.evaluate(x, y).s

    example8 = compile(Example8)
    for x in uints:
        for y in uints:
            assert (x - y) & mask == example8.evaluate(x, y).s

    example10 = compile(Example10)
    for x in uints:
        for y in uints:
            ltu = example10.evaluate(x, y).ltu
            assert (x < y) == ltu

    for x in sints:
        for y in sints:
            lts = example10.evaluate(x, y).lts
            assert (x < y) == lts

    example11 = compile(Example11)
    for x in uints:
        for y in uints:
            s = example11.evaluate(x, y).s
            assert (x + y) & mask == s

    example12 = compile(Example12)
    for x in uints:
        for y in uints:
            s = example12.evaluate(x, y).s
            assert (x + y) & mask == s
    
    example13 = compile(Example13, trace=False)
    for x in uints:
        for y in uints:
            s = example13.evaluate(x, y).s
            assert (x + y) & mask == s

    example15 = compile(Example15)
    for x in uints:
        for y in uints:
            s = example15.evaluate(x, y).s
            assert (x + y) & mask == s
