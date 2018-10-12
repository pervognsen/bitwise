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
class Add2:
    x = input(bit)
    y = input(bit)
    s = output(x ^ y)
    c = output(x & y)

def add2(x, y):
    adder = Add2(x=x, y=y)
    return adder.s, adder.c

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
            x = when(n[i], x[-m:] @ x[:-m], x)
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
    return bits(when(~i >= n, b, x[-1]) for i, b in enumerate(y))

def barrel_shifter(x, n, dir, shift, arith, left_rotator=left_rotator_radix4):
    x, n, dir, shift, arith = trace(x, base=2), trace(n), trace(dir), trace(shift), trace(arith)
    y = left_rotator(when(dir, x[-1] @ x[:-1], x), when(dir, ~n, n))
    mask = bits(~shift | when(dir, ~i >= n, i >= n) for i in range(len(y)))
    mask = trace(mask, 'mask', base=2)
    y = bits(when(mask[i], b, arith & x[-1]) for i, b in enumerate(y))
    y = trace(y, 'y', base=2)
    return y

@module
class Example16:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(left_shifter_radix2(x, n))

@module
class Example17:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(left_shifter_radix4(x, n))

@module
class Example18:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(left_rotator_radix4(x, n))

@module
class Example19:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(right_rotator_radix2(x, n))

@module
class Example20:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(barrel_left_shifter(x, n))

@module
class Example21:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(barrel_logical_right_shifter(x, n))

@module
class Example22:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(barrel_arithmetic_right_shifter(x, n))

@module
class Example23:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    dir = input(bit)
    shift = input(bit)
    arith = input(bit)
    y = output(barrel_shifter(x, n, dir, shift, arith))

def funnel_shifter_radix2(x, n):
    assert 2 * 2**len(n) - 1 == len(x)
    for i, b in reversed(list(enumerate(n))):
        x = when(b, x[2**i:], x[:-2**i])
    assert 2**len(n) == len(x)
    return x

# def funnel_shifter_radix4(x, n):
#     assert 2 * 2**len(n) - 1 == len(x)
#     for i in range(0, len(n), 2):
#         m = 2**i
#         if i+2 <= len(n):
#             x = mux4(n[i:i+2], x[:-3*m], x[m:-2*m], x[2*m:-m], x[3*m:])
#         else:
#             x = when(b, x[m:], x[:-m])
#     assert 2**len(n) == len(x)
#     return x

funnel_shifter = funnel_shifter_radix2

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

def funnel_shifter_unit(x, n, dir, shift, arith):
    s = rep(arith & x[-1], len(x)-1)
    low = when(dir, x[:-1], when(shift, s, x[1:]))
    mid = when(dir, x[-1], x[0])
    high = when(dir, when(shift, s, x[:-1]), x[1:])
    return funnel_shifter(low @ mid @ high, when(dir, n, ~n))

@module
class Example24:
    x = input(bit[2*N - 1])
    n = input(bit[clog2(N)])
    y = output(funnel_shifter(x, n))

@module
class Example25:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(funnel_right_shifter(x, n))

@module
class Example26:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(funnel_left_shifter(x, n))

@module
class Example27:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(funnel_arithmetic_right_shifter(x, n))

@module
class Example28:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(funnel_right_rotator(x, n))

@module
class Example29:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    y = output(funnel_left_rotator(x, n))

def right_shifter_radix2(x, n, arith):
    s = arith & x[-1]
    for i, b in enumerate(n):
        m = 2**i
        x = when(b, x[m:] @ rep(s, m), x)
    return x

def right_shifter_radix4(x, n, arith):
    s = arith & x[-1]
    for i in range(0, len(n), 2):
        m = 2**i
        if i+2 <= len(n):
            x = mux4(n[i:i+2], x, x[m:] @ rep(s, m), x[2*m:] @ rep(s, 2*m), x[3*m:] @ rep(s, 3*m))
        else:
            x = when(n[i], x[m:] @ rep(s, m), x)
    return x

@module
class Example30:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    arith = input(bit)
    y = output(right_shifter_radix4(x, n, arith))

def simple_shifter_unit(x, n, dir, arith, left_shifter=left_shifter_radix4, right_shifter=right_shifter_radix4):
    return when(dir, right_shifter(x, n, arith), left_shifter(x, n))

@module
class Example31:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    dir = input(bit)
    arith = input(bit)
    y = output(simple_shifter_unit(x, n, dir, arith))

@module
class Example32:
    x = input(bit[N])
    n = input(bit[clog2(N)])
    dir = input(bit)
    shift = input(bit)
    arith = input(bit)
    y = output(funnel_shifter_unit(x, n, dir, shift, arith))

adder = lambda x, y: x + y

def weighted_partial_products(x, y):
    assert len(x) == len(y)
    return [(j, bits(label(xi & yj, i+j) for i, xi in enumerate(x))) for j, yj in enumerate(y)]

def partial_products(x, y):
    return [pp << i for i, pp in weighted_partial_products(x, y)]

def naive_multiplier(x, y):
    # return sum(partial_products(x, y))
    return binary_reduce(adder, partial_products(x, y))

@module
class Example33:
    x = input(bit[N])
    y = input(bit[N])
    p = output(naive_multiplier(x, y))

# invariant: for s, c = csa(x, y, z), x + y + z == s + (c << 1)
def csa(x, y, z):
    sc = [add3(xi, yi, zi) for xi, yi, zi in zip(x, y, z)]
    return bits(si for si, ci in sc), bits(ci for si, ci in sc) << 1

def array_multiadder(xs):
    assert len(xs) >= 2
    x, y = xs[0], xs[1]
    for z in xs[2:]:
        x, y = csa(x, y, z)
    return adder(x, y)

def array_binary_multiadder(xs):
    assert len(xs) >= 2
    while len(xs) > 2:
        ys = []
        n = len(xs)
        for i in range(0, n, 3):
            if i+3 <= n:
                ys.extend(csa(*xs[i:i+3]))
            else:
                ys.extend(xs[i:i+2])
        xs = ys
    return xs[0] + xs[1]

def array_multiplier(x, y):
    return array_multiadder(partial_products(x, y))

@module
class Example34:
    x = input(bit[N])
    y = input(bit[N])
    p = output(array_multiplier(x, y))

def wallace_tree_multiadder(weighted_terms, max_bits):
    def put(i, x):
        if i < max_bits:
            next_pending[i].append(x)

    next_pending = [[] for i in range(max_bits)]
    for i, x in weighted_terms:
        for j, b in enumerate(x):
            put(i+j, b)

    while any(len(bs) > 2 for bs in next_pending):
        pending, next_pending = next_pending, [[] for i in range(max_bits)]
        for i, bs in enumerate(pending):
            n = len(bs)
            for k in range(0, n, 3):
                if k+3 <= n:
                    s, c = add3(*bs[k:k+3])
                    put(i, label(s, i))
                    put(i+1, label(c, i+1))
                elif k+2 <= n:
                    s, c = add2(*bs[k:k+2])
                    put(i, label(s, i))
                    put(i+1, label(c, i+1))
                else:
                    put(i, bs[k])

    x = bits(bs[0] if 0 < len(bs) else 0 for bs in next_pending)
    y = bits(bs[1] if 1 < len(bs) else 0 for bs in next_pending)
    return x + y

def wallace_tree_multiplier(x, y):
    return wallace_tree_multiadder(weighted_partial_products(x, y), len(x))

@module
class Example35:
    x = input(bit[N])
    y = input(bit[N])
    p = output(wallace_tree_multiplier(x, y))

def sw_euclidean_divide(n, d):
    q = 0
    r = n
    while r >= d:
        q += 1
        r -= d
    assert n == q*d + r
    assert 0 <= r < d
    return q

def sw_binary_divide(n, d, num_bits):
    q = 0
    r = n
    d2 = d << (num_bits - 1)
    for i in range(num_bits):
        q <<= 1
        if r >= d2:
            q |= 1
            r -= d2
        d2 >>= 1
    assert n == q*d + r
    assert 0 <= r < d
    return q

def sw_binary_divide2(n, d, num_bits):
    q = 0
    r = n
    d2 = d << (num_bits - 1)
    for i in range(num_bits):
        q <<= 1
        if r >= d2:
            q |= 1
            r -= d2
        r <<= 1
    return q

def binary_divider(n, d):
    q = bit[0](0)
    r = n @ bit[len(d)-1](0)
    d2 = bit[len(d)-1](0) @ d
    for i in range(len(d)):
        q, r = when(r >= d2, (1 @ q, r - d2), (0 @ q, r))
        r <<= 1
    return q

# Restoring division
def binary_divider2(n, d):
    q = bit[0](0)
    r = n @ bit[len(d)](0)
    d2 = bit[len(d)-1](0) @ d @ 0
    for i in range(len(d)):
        r2 = r - d2
        q, r = when(r2[-1], (0 @ q, r), (1 @ q, r2))
        r <<= 1
    return q

@module
class Example36:
    n = input(bit[N])
    d = input(bit[N])
    q = output(binary_divider2(n, d))
    assert len(q) == N

def sw_nonrestoring_divide(n, d, num_bits):
    q = 0
    r = n
    d2 = d << (num_bits - 1)
    for i in range(num_bits):
        q <<= 1
        if r >= 0:
            q |= 1
            r -= d2
        else:
            r += d2
        r <<= 1
    q = q - ~q
    if r < 0:
        q -= 1
    return q & ((1 << num_bits) - 1)

# Non-restoring division
def nonrestoring_binary_divider(n, d):
    q = bit[0](0)
    r = n @ bit[len(d)](0) @ 0
    d2 = bit[len(d)-1](0) @ d @ 0 @ 0
    for i in range(len(d)):
        q, r = when(r[-1], (0 @ q, r + d2), (1 @ q, r - d2))
        r <<= 1
    # q = q - ~q
    # q = when(r[-1], q - 1, q)
    q = when(r[-1], q << 1, q - ~q)
    return q

@module
class Example37:
    n = input(bit[N])
    d = input(bit[N])
    q = output(nonrestoring_binary_divider(n, d))
    assert len(q) == N

@module
class Example38:
    enable = input(bit)
    counter = register(bit[N])
    counter.next = when(enable, counter + 1, counter)
    value = output(counter)

@module
class Example39:
    enable = input(bit)
    x = input(bit[N])
    y = input(bit[N])

    x_reg = register(bit[N])
    y_reg = register(bit[N])
    p_valid_reg = register(bit)
    p_reg = register(bit[N])

    x_reg.next = when(enable, x, x_reg >> 1)
    y_reg.next = when(enable, y, y_reg << 1)
    p_reg.next = when(enable, 0, p_reg + when(x_reg[0], y_reg, 0))
    p_valid_reg.next = when(enable, 0, x_reg.next == 0)

    p = output(p_reg)
    p_valid = output(p_valid_reg)

@module
class Example40:
    x = input(bit[N])
    y = output(delay(x))

def combinational_multiplier(x, y):
    p = 0
    for i in range(len(x)):
        x, y, p = x >> 1, y << 1, p + when(x[0], y, 0)
    return p

@module
class Example41:
    x = input(bit[N])
    y = input(bit[N])
    p = output(combinational_multiplier(x, y))

def pipelined_multiplier(x, y, enable):
    p, p_valid = 0, enable
    for i in range(len(x)):
        x, y, p, p_valid = delay((x >> 1, y << 1, p + when(x[0], y, 0), p_valid))
    return p, p_valid

@module
class Example42:
    x = input(bit[N])
    y = input(bit[N])
    enable = input(bit)
    p, p_valid = pipelined_multiplier(x, y, enable)
    p = output(p)
    p_valid = output(p_valid)
    # p, p_valid = output(pipelined_multiplier(x, y, enable))


def mux(addr, data):
    assert ispow2(len(data))
    if len(data) == 1:
        return data[0]
    else:
        i = len(data) // 2
        return when(addr[-1], mux(addr[:-1], data[i:]), mux(addr[:-1], data[:i]))

@module
def memory(data_type, size):
    assert ispow2(size)
    addr_type = bit[clog2(size)]

    cells = [register(data_type) for i in range(size)]

    write_enable = input(bit)
    write_addr = input(addr_type)
    write_data = input(data_type)

    read_addr = input(addr_type)

    # Read-after-write semantics: Bypass write data to read data if addresses match.
    read_data = output(delay(when(write_enable & (read_addr == write_addr), write_data, mux(read_addr, cells))))

    for i, cell in enumerate(cells):
        cell.next = when(write_enable & (write_addr == i), write_data, cell)

def rotl(x, n):
    return ((x << n) & mask) | ((x >> (N - n)) & mask)

def rotr(x, n):
    return ((x >> n) & mask) | ((x << (N - n)) & mask)

def simulate_test(sim, *tests):
    sim_inst = sim()
    testers = set(test(sim_inst) for test in tests)
    while testers:
        stopped = set()
        for tester in testers:
            try:
                next(tester)
            except StopIteration:
                stopped.add(tester)

        sim_inst.tick()
        sim_inst.update()

        testers.difference_update(stopped)

@module
def fifo(data_type, size):
    assert ispow2(size)

    mem = memory(data_type, size)()

    enqueue_enable = input(bit)
    enqueue_data = input(data_type)
    dequeue_enable = input(bit)

    addr_type = bit[clog2(size)]    
    read_addr = register(addr_type)
    write_addr = register(addr_type)

    not_empty = read_addr != write_addr
    not_full = write_addr + 1 != read_addr

    enqueue_ready = output(not_full)
    dequeue_ready = output(not_empty)

    read_addr.next = when(dequeue_enable & dequeue_ready, read_addr + 1, read_addr)
    write_addr.next = when(enqueue_enable & enqueue_ready, write_addr + 1, write_addr)

    mem.read_addr = read_addr.next
    dequeue_data = output(mem.read_data)

    mem.write_enable = enqueue_enable & enqueue_ready
    mem.write_addr = write_addr
    mem.write_data = enqueue_data

open('example.dot', 'w').write(generate_dot_file(Example42))

do_timing_analysis = False
if do_timing_analysis:
    print("Ripple-carry:", analyze_delay(Example7))
    print("Recursive ripple-carry:", analyze_delay(Example11))
    print("Carry-select:", analyze_delay(Example12))
    print("Conditional sum:", analyze_delay(Example13))

    for scan in (linear_scan, naive_logarithmic_scan, sklansky_scan, brent_kung_scan, kogge_stone_scan):
        print("Carry-lookahead with %s:" % scan.__name__, analyze_delay(make_carrylookahead_tester(scan)))

mask = (1 << N) - 1

uints = range(2**N)
sints = range(-2**(N-1), 2**(N-1))
shifts = range(N)
fints = range(2**(2*N - 1))
 
print(analyze_delay(Example36))
print(analyze_delay(Example37))

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

    # example15 = compile(Example15)
    # for x in uints:
    #     for y in uints:
    #         s = example15.evaluate(x, y).s
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

    # example24 = compile(Example24)
    # for x in fints:
    #     for n in shifts:
    #         y = example24.evaluate(x, n).y
    #         assert (x >> n) & mask == y

    # example25 = compile(Example25)
    # for x in uints:
    #     for n in shifts:
    #         y = example25.evaluate(x, n).y
    #         assert (x >> n) & mask == y

    # example26 = compile(Example26)
    # for x in uints:
    #     for n in shifts:
    #         y = example26.evaluate(x, n).y
    #         assert (x << n) & mask == y

    # example27 = compile(Example27)
    # for x in sints:
    #     for n in shifts:
    #         y = example27.evaluate(x, n).y
    #         assert (x >> n) & mask == y

    # example28 = compile(Example28)
    # for x in uints:
    #     for n in shifts:
    #         y = example28.evaluate(x, n).y
    #         assert rotr(x, n) == y

    # example29 = compile(Example29)
    # for x in uints:
    #     for n in shifts:
    #         y = example29.evaluate(x, n).y
    #         assert rotl(x, n) == y

    # example30 = compile(Example30)
    # for x in uints:
    #     for n in shifts:
    #         y = example30.evaluate(x, n, arith=0).y
    #         assert (x >> n) & mask == y
    # for x in sints:
    #     for n in shifts:
    #         y = example30.evaluate(x, n, arith=1).y
    #         assert (x >> n) & mask == y

    # example31 = compile(Example31)
    # for x in uints:
    #     for n in shifts:
    #         y = example31.evaluate(x, n, dir=0, arith=0).y
    #         assert (x << n) & mask == y
    # for x in uints:
    #     for n in shifts:
    #         y = example31.evaluate(x, n, dir=1, arith=0).y
    #         assert (x >> n) & mask == y
    # for x in sints:
    #     for n in shifts:
    #         y = example31.evaluate(x, n, dir=1, arith=1).y
    #         assert (x >> n) & mask == y

    # example32 = compile(Example32)
    # for x in uints:
    #     for n in shifts:
    #         y = example32.evaluate(x, n, dir=0, shift=0, arith=0).y
    #         assert rotl(x, n) == y
    # for x in uints:
    #     for n in shifts:
    #         y = example32.evaluate(x, n, dir=1, shift=0, arith=0).y
    #         assert rotr(x, n) == y
    # for x in uints:
    #     for n in shifts:
    #         y = example32.evaluate(x, n, dir=0, shift=1, arith=0).y
    #         assert (x << n) & mask == y
    # for x in uints:
    #     for n in shifts:
    #         y = example32.evaluate(x, n, dir=1, shift=1, arith=0).y
    #         assert (x >> n) & mask == y
    # for x in sints:
    #     for n in shifts:
    #         y = example32.evaluate(x, n, dir=1, shift=1, arith=1).y
    #         assert (x >> n) & mask == y

    # example33 = compile(Example33)
    # for x in uints:
    #     for y in uints:
    #         p = example33.evaluate(x, y).p
    #         assert (x * y) & mask == p
    # for x in sints:
    #     for y in sints:
    #         p = example33.evaluate(x, y).p
    #         assert (x * y) & mask == p

    # example34 = compile(Example34)
    # for x in uints:
    #     for y in uints:
    #         p = example34.evaluate(x, y).p
    #         assert (x * y) & mask == p
    # for x in sints:
    #     for y in sints:
    #         p = example34.evaluate(x, y).p
    #         assert (x * y) & mask == p

    # example35 = compile(Example35)
    # for x in uints:
    #     for y in uints:
    #         p = example35.evaluate(x, y, trace=True).p
    #         assert (x * y) & mask == p
    # for x in sints:
    #     for y in sints:
    #         p = example35.evaluate(x, y).p
    #         assert (x * y) & mask == p

    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = sw_euclidean_divide(n, d)
    #             assert q == n // d

    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = sw_binary_divide(n, d, N)
    #             assert q == n // d

    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = sw_binary_divide2(n, d, N)
    #             assert q == n // d

    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = sw_nonrestoring_divide(n, d, N)
    #             assert q == n // d

    # example36 = compile(Example36)
    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = example36.evaluate(n, d).q
    #             assert q == n // d

    # example37 = compile(Example37)
    # for n in uints:
    #     for d in uints:
    #         if d != 0:
    #             q = example37.evaluate(n, d, trace=True).q
    #             assert q == n // d

    # example38 = compile(Example38)

    # example38_instance = example38()
    # for i in range(10):
    #     example38_instance.enable = i % 2
    #     example38_instance.tick()
    #     example38_instance.update()
    #     print("i = %s, value = %s" % (i, example38_instance.value))

    # example38_instance = example38()
    # example38_instance.enable = 1
    # for i, outputs in zip(range(20), example38_instance):
    #     print("i = %s, value = %s" % (i, outputs.value))

    # example39 = compile(Example39)

    # example39_instance = example39()
    # for x in uints:
    #     for y in uints:
    #         for i, outputs in enumerate(example39_instance):
    #             if i == 0:
    #                 example39_instance.x = x
    #                 example39_instance.y = y
    #                 example39_instance.enable = 1
    #             else:
    #                 example39_instance.enable = 0

    #                 if example39_instance.p_valid:
    #                     break

    #         assert example39_instance.p == (x * y) & mask

    # def example39_test(self):
    #     for x in uints:
    #         for y in uints:
    #             self.x = x
    #             self.y = y
    #             self.enable = 1
    #             yield

    #             self.enable = 0
    #             while not self.p_valid:
    #                 yield

    #             assert self.p == (x * y) & mask

    # simulate_test(example39, example39_test)

    # example40 = compile(Example40)

    # def example40_test(self):
    #     for x in uints:
    #         self.x = x
    #         yield
    #         assert self.y == x

    # simulate_test(example40, example40_test)

    # example41 = compile(Example41)

    # for x in uints:
    #     for y in uints:
    #         p = example41.evaluate(x, y).p
    #         assert p == (x * y) & mask

    # example42 = compile(Example42)

    # def example42_test(self):
    #     for x in uints:
    #         for y in uints:
    #             self.x = x
    #             self.y = y
    #             self.enable = 1
    #             yield

    #             self.enable = 0
    #             while not self.p_valid:
    #                 yield

    #             assert self.p == (x * y) & mask

    # simulate_test(example42, example42_test)

    # def example42_test_producer(self):
    #     for x in uints:
    #         for y in uints:
    #             self.x = x
    #             self.y = y
    #             self.enable = 1
    #             yield

    #     self.enable = 0

    # def example42_test_consumer(self):
    #     for x in uints:
    #         for y in uints:
    #             while not self.p_valid:
    #                 yield

    #             assert self.p == (x * y) & mask
    #             yield

    # simulate_test(example42, example42_test_producer, example42_test_consumer)

    # memory_example = compile(memory(bit[8], 64))

    # def memory_example_test(self):
    #     def scramble(i):
    #         return (i * 0xdeadbeef) & 0xff

    #     for i in range(64):
    #         self.read_addr = i
    #         yield
    #         assert self.read_data == 0

    #     for i in range(64):
    #         self.write_enable = 1
    #         self.write_addr = i
    #         self.write_data = scramble(i)
    #         yield

    #     self.write_enable = 0
        
    #     for i in range(64):
    #         self.read_addr = i
    #         yield
    #         assert self.read_data == scramble(i)

    # simulate_test(memory_example, memory_example_test)
    
    fifo_example = compile(fifo(bit[8], 64))

    def fifo_test_producer(self):
        yield
        for i in range(256):
            self.enqueue_enable = 1
            self.enqueue_data = i
            while not self.enqueue_ready:
                yield

            yield
        
        self.enqueue_enable = 0

    def fifo_test_consumer(self):
        # for i in range(100):
        #     yield

        for i in range(256):
            self.dequeue_enable = 1
            while not self.dequeue_ready:
                yield

            assert self.dequeue_data == i
            yield

        self.dequeue_enable = 0

    simulate_test(fifo_example, fifo_test_producer, fifo_test_consumer)
