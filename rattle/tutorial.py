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

@module
class Example7:
    x = input(bit[4])
    y = input(bit[4])
    s = output(add(x, y))

def sub(x, y):
    return add(x, ~y, 1)

@module
class Example8:
    x = input(bit[4])
    y = input(bit[4])
    s = output(sub(x, y))

@module
class Example9:
    x = input(bit[4])
    y = input(bit[4])
    n = input(bit)
    s = output(add(x, when(n, ~y, y), n))

@module
class Example10:
    x = input(bit[4])
    y = input(bit[4])
    s, c = adc(x, ~y, 1)
    # less than unsigned
    ltu = output(c)
    # less than signed
    lts = output(c ^ x[-1] ^ ~y[-1])

open('example.dot', 'w').write(generate_dot_file(Example9))
