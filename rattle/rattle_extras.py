from rattle import *

@module
class FA:
    in1 = input(bit)
    in2 = input(bit)
    cin = input(bit)
    p = in1 ^ in2
    g = in1 & in2
    out = output(p ^ cin)
    cout = output(g | p & cin)

def full_adder(x, y, z):
    adder = FA(in1=x, in2=y, cin=z)
    return adder.out, adder.cout

@module
class HA:
    in1 = input(bit)
    in2 = input(bit)
    out = output(in1 ^ in2)
    cout = output(in1 & in2)

def half_adder(x, y):
    adder = HA(in1=x, in2=y)
    return adder.out, adder.cout

@module
class Adder8:
    in1 = input(bit[8])
    in2 = input(bit[8])
    cin = input()

    def add(in1, in2, c):
        for x, y in zip(in1, in2):
            s, c = full_adder(x, y, c)
            yield s
        yield c

    s = bits(add(in1, in2, cin))

    out = output(s[:-1])
    cout = output(s[-1])

@module
class Adder16:
    in1 = input(bit[16])
    in2 = input(bit[16])
    cin = input()

    adder1 = Adder8(in1=in1[:8], in2=in2[:8], cin=cin)
    adder2 = Adder8(in1=in1[8:], in2=in2[8:], cin=adder1.cout)

    # out = output(bit[16])
    # cout = output(bit)

    # out.connect([adder1.out, adder2.out])
    # cout.connect(adder2.cout)

    # adder1 = Adder8(in1=in1[:8], in2=in2[:8], cin=cin, out=out[:8])
    # adder2 = Adder8(in1=in1[8:], in2=in2[8:], cin=adder1.cout, out=out[8:], cout=cout)

    # adder1 = Adder8()
    # adder2 = Adder8()
    
    # adder1.in1 = in1[:8]
    # adder1.in2 = in2[:8]
    # adder1.cin = cin
 
    # adder2.in1 = in1[8:]
    # adder2.in2 = in2[8:]
    # adder2.cin = adder1.cout

    out = output([adder1.out, adder2.out])
    cout = output(adder2.cout)

def reduce(xs, f):
    if len(xs) == 1:
        return xs[0]
    else:
        n = len(xs) // 2
        return f(reduce(xs[:n], f), reduce(xs[n:], f))

def parity(xs):
    return reduce(xs, lambda x, y: x ^ y)

def scan(xs, f):
    def helper(xs, f):
        y = xs[0]
        yield y
        for x in xs[1:]:
            y = f(y, x)
            yield y
    return list(helper(xs, f))

def scan2(xs, f):
    if len(xs) == 1:
        return [xs[0]]
    else:
        n = len(xs) // 2
        ys = scan2(xs[:n], f)
        return ys + [f(ys[-1], y) for y in scan2(xs[n:], f)]

def unzip(xs, ys):
    assert len(xs) == len(ys)
    def helper(xs, ys):
        for x, y in zip(xs, ys):
            yield x
            yield y
    return list(helper(xs, ys))

def scan3(xs, f):
    if len(xs) == 1:
        return [xs[0]]
    else:
        ys = scan3([f(x0, x1) for x0, x1 in zip(xs[::2], xs[1::2])], f)
        return unzip([xs[0]] + [f(y, x) for x, y in zip(xs[2::2], ys)], ys)

def scan4(xs, f):
    xs = list(xs)
    i = 1
    while i < len(xs):
        xs = [buf(x) for x in xs[:i]] + [f(x0, x1) for x0, x1 in zip(xs, xs[i:])]
        i *= 2
    return xs

def segscan(xs, bs, f):
    if len(xs) == 1:
        return [xs[0]]
    else:
        n = len(xs) // 2
        ys = segscan(xs[:n], bs[:n], f)
        return ys + [when(bs[n], y, f(ys[-1], y)) for y in segscan(xs[n:], bs[n:], f)]

def segscan2(xs, bs, f):
    if len(xs) == 1:
        return [xs[0]]
    else:
        ys = segscan2([when(b, x1, f(x0, x1)) for x0, x1, b in zip(xs[::2], xs[1::2], bs[1::2])], bs[1::2], f)
        return unzip([xs[0]] + [when(b, x, f(y, x)) for x, y, b in zip(xs[2::2], ys, bs[2::2])], ys)

@module
class Parity8:
    x = input(bit[8])
    y = output(parity(x))

@module
class LinearXorScanner8:
    x = input(bit[8])
    y = output(bits(scan(x, lambda x, y: x ^ y)))

@module
class LogarithmicXorScanner8:
    x = input(bit[8])
    y = output(bits(scan4(list(x), lambda x, y: x ^ y)))

@module
class Counter:
    enable = input(bit)
    count = register(bit[8])
    count.next = when(enable, count + 1, count)
    #count.enable = enable
    #count.next = count + 1
    out = output(count)

@module
class Sort2:
    i0 = input(bit[8])
    i1 = input(bit[8])
    o0 = output(when(i0 <= i1, i0, i1))
    o1 = output(when(i0 <= i1, i1, i0))

def sort2(x, i, j, d):
    m = Sort2(i0=x[i], i1=x[j])
    if d:
        x[i], x[j] = m.o0, m.o1
    else:
        x[j], x[i] = m.o0, m.o1

def merge(x, i, j, d):
    n = (j - i) // 2
    if n > 0:
        for k in range(i, i + n):
            sort2(x, k, k + n, d)
        merge(x, i, i + n, d)
        merge(x, i + n, j, d)

def sort(x, i, j, d):
    n = (j - i) // 2
    if n > 0:
        sort(x, i, i + n, True)
        sort(x, i + n, j, False)
        merge(x, i, j, d)

N = 32

@module
class BitonicSorter:
    inputs = {'i%d' % i: input(bit[8]) for i in range(N)}
    values = list(inputs.values())
    sort(values, 0, N, True)
    outputs = (lambda values=values: {'o%d' % i: output(values[i]) for i in range(N)})()

def popcount(x):
    n = len(x).bit_length()
    queues = {i: [] for i in range(n)}
    queues[0] = list(x)
    for i, queue in queues.items():
        while len(queue) != 1:
            if len(queue) == 2:
                s, c = half_adder(queue[0], queue[1])
                del queue[:2]
                queue.append(s)
                queues[i+1].append(c)
            elif len(queue) >= 3:
                s, c = full_adder(queue[0], queue[1], queue[2])
                del queue[:3]
                queue.append(s)
                queues[i+1].append(c)
    return bits(queue[0] for queue in queues.values())

def popcount2(x):
    n = len(x).bit_length()
    queues = {weight: [] for weight in range(n)}
    queues[0] = list(x)
    while any(len(queue) > 2 for queue in queues.values()):
        new_queues = {weight: [] for weight in range(n)}
        for weight, queue in queues.items():
            for i in range(0, len(queue), 3):
                if i+2 < len(queue):
                    s, c = full_adder(queue[i], queue[i+1], queue[i+2])
                    new_queues[weight].append(s)
                    new_queues[weight+1].append(c)
                elif i+1 < len(queue):
                    s, c = half_adder(queue[i], queue[i+1])
                    new_queues[weight].append(s)
                    new_queues[weight+1].append(c)
                else:
                    new_queues[weight].append(queue[i])
        queues = new_queues
    left = bits((queue[0] if len(queue) >= 1 else 0) for queue in queues.values())
    right = bits((queue[1] if len(queue) >= 2 else 0) for queue in queues.values())
    return left + right

def popcount3(x):
    return reduce(x, lambda m, n: m @ 0 + n @ 0)

@module
class PopCount:
    n = 16
    x = input(bit[n])
    y = output(popcount3(x))

def add1(x, y, z):
    return full_adder(x, y, z)

def add(x, y, c):
    if len(x) == 1:
        return add1(x[0], y[0], c)
    else:
        n = len(x) // 2
        lo, c = add(x[:n], y[:n], c)
        hi, c = when(c, add(x[n:], y[n:], 1), add(x[n:], y[n:], 0))
        return lo @ hi, c

@module
class Adder32:
    x = input(bit[8])
    y = input(bit[8])
    s, c = add(x, y, 0)
    s = output(s)

def draw_digits(digits):
    def c(x, b): return x if b else ' '
    line1 = ' '.join(' ' + c('_', d[0]) + ' ' for d in digits)
    line2 = ' '.join(c('|', d[1]) + c('_', d[2]) + c('|', d[3]) for d in digits)
    line3 = ' '.join(c('|', d[4]) + c('_', d[5]) + c('|', d[6]) for d in digits)
    return "%s\n%s\n%s\n" % (line1, line2, line3)

def make_digit_counter(enable):
    digit = register(bit[4], enable=enable)
    carry = digit == 9
    digit.next = when(carry, 0, digit + 1)
    return digit, enable & carry

def make_multidigit_counter(num_digits, enable):
    digits = []
    for i in range(num_digits):
        digit, enable = make_digit_counter(enable)
        digits.append(digit)
    return digits

@module
def DigitCounter():
    enable = input(bit)
    digits = make_multidigit_counter(2, enable)
    digit0 = output(digits[0])
    digit1 = output(digits[1])

def one_bit_index(xs):
    def combine(y0, y1):
        valid0, index0 = y0
        valid1, index1 = y1
        return valid0 | valid1, when(valid0, index0 @ 0, index1 @ 1)
    return reduce([(x, empty) for x in xs], combine)

def cam(values, key):
    return one_bit_index([value == key for value in values])

# def one_bit_index(x):
#     if len(x) == 1:
#         return x[0], empty
#     else:
#         n = len(x) // 2
#         valid0, index0 = one_bit_index(x[:n])
#         valid1, index1 = one_bit_index(x[n:])
#         return valid0 | valid1, when(valid0, index0 @ 0, index1 @ 1)

@module
class CAM:
    n = 8
    value = input(bit[8])
    values = [register(bit[8]) for i in range(n)]
    valid, index = cam(values, value)
    valid = output(valid)
    index = output(index)

def delay(next, init=None):
    next = as_node(next)
    return register(next.type, init=init, next=next)

def multiply(x, y):
    assert len(x) == len(y)
    n = len(x)
    p = bit[n](0)
    for i in range(n):
        p += (x & rep(y[i], n)) << i
    return p

def multiply2(x, y):
    assert len(x) == len(y)
    n = len(x)
    p = bit[n](0)
    for i in range(n):
        p = p + x & rep(y[0], n)
        x = x << 1
        y = y >> 1
    return p

def multiply3(x, y):
    assert len(x) == len(y)
    n = len(x)
    p = bit[n](0)
    for i in range(n):
        p = delay(p + x & rep(y[0], n))
        x = delay(x << 1)
        y = delay(y >> 1)
    return p

@module
class Multiplier:
    x = input(bit[8])
    y = input(bit[8])
    p = output(multiply3(x, y))

@module
class SerialMultiplier:
    n = 8
    x0 = input(bit[n])
    y0 = input(bit[n])
    start = input(bit)

    i = register(bit[(n - 1).bit_length()])
    p = register(bit[n])
    x = register(bit[n])
    y = register(bit[n])

    done = delay(i == n - 1)

    i.enable = p.enable = x.enable = y.enable = start | ~done

    p.next = when(start, 0, p + x & rep(y[0], n))
    x.next = when(start, x0, x << 1)
    y.next = when(start, y0, y >> 1)
    i.next = when(start, 0, i + 1)

    p = output(p)
    done = output(done)

@module
class FPAdder:
    x_e = input(bit[8])
    x_m = input(bit[24])
    y_e = input(bit[8])
    y_m = input(bit[24])

    def add(x_e, x_m, y_e, y_m):
        x_e, x_m, y_e, y_m = when(x_e >= y_e, (x_e, x_m, y_e, y_m), (y_e, y_m, x_e, x_m))
        s = x_m @ bit[2](1) + ((y_m @ bit[2](1)) >> (x_e - y_e))
        return when(s[-1], (x_e + 1, s[1:-1]), (x_e, s[:-2]))

    e, m = add(x_e, x_m, y_e, y_m)

    s_e = output(e)
    s_m = output(m)

@module
class PG:
    p1 = input(bit)
    g1 = input(bit)
    p2 = input(bit)
    g2 = input(bit)
    p = output(p1 & p2)
    g = output((g1 & p2) | g2)

def pg_combine(pg1, pg2):
    p1, g1 = pg1
    p2, g2 = pg2
    pg = PG(p1=p1, g1=g1, p2=p2, g2=g2)
    return pg.p, pg.g

def fast_add(xs, ys, scan=scan):
    pgs = [(x ^ y, x & y) for x, y in zip(xs, ys)]
    return [pgs[0][0]] + [p ^ c for (p, g), (_, c) in zip(pgs[1:], scan(pgs, pg_combine))]

@module
class FastAdder:
    x = input(bit[8])
    y = input(bit[8])
    s = output(fast_add(x, y, scan2))

def fast_simd_add(xs, ys, bs, segscan=segscan):
    pgs = [(x ^ y, x & y) for x, y in zip(xs, ys)]
    bs = list(bs)
    return [pgs[0][0]] + [p ^ c for (p, g), (_, c) in zip(pgs[1:], segscan(pgs, bs, pg_combine))]

@module
class FastSIMDAdder:
    x = input(bit[8])
    y = input(bit[8])
    b = input(bit[8])
    s = output(fast_simd_add(x, y, b, segscan2))

from functools import total_ordering

@total_ordering
class Bundle:
    def __init__(self, keys, values):
        self.keys = tuple(keys)
        self.values = tuple(values)

    def __iter__(self):
        return iter(self.values)

    def __len__(self):
        return len(self.values)

    def __hash__(self):
        return hash((self.keys, self.values))

    def __eq__(self, other):
        if isinstance(other, Bundle) and self.keys == other.keys:
            return self.values == other.values
        return NotImplemented

    def __lt__(self, other):
        if isinstance(other, Bundle) and self.keys == other.keys:
            return self.values < other.values
        return NotImplemented

    def __getitem__(self, i):
        return self.values[i]

    def __call__(self, *args, **kwargs):
        values = list(self.values)
        for i, value in enumerate(args):
            values[i] = value
        for key, value in kwargs.items():
            values[self.keys.index(key)] = value
        return Bundle(self.keys, values)

    def __getattr__(self, name):
        try:
            return self.values[self.keys.index(name)]
        except ValueError:
            return super().__getattr__(name)

    def __repr__(self):
        return 'bundle(%s)' % (', '.join((key + '=' if type(key) == str else '') + repr(value) for key, value in zip(self.keys, self.values)))

def bundle(*args, **kwargs):
    keys = tuple(range(len(args))) + tuple(kwargs.keys())
    values = tuple(args) + tuple(kwargs.values())
    return Bundle(keys, values)

# @bundle
class float32:
    sign: bit
    exponent: bit[8]
    mantissa: bit[23]

def csa_array(xs):
    s, c = xs[0], xs[1]
    for x in xs[2:]:
        p, g = s ^ c, s & c
        s, c = p ^ x, g | (p & x)
        c = 0 @ c[:-1]
    return s + c

@module
class Adder3:
    word = bit[8]
    x = input(word)
    y = input(word)
    z = input(word)
    w = input(word)
    s = output(csa_array([x, y, z, w]))

b1 = bundle(name='Per', age=35)
b2 = bundle(age=35, name='Per')
b3 = b1(name='Mon')(age=36)
print(b1)
print(b2)
print(b1 > b3)
print(b3)
print(bundle(1, 2, 3))

dot_file = open('example.dot', 'w')
dot_file.write(generate_dot_file(Adder3))
