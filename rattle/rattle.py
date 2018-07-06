from collections.abc import Iterable

def memo(f):
    table = {}
    def memo_f(*args):
        try:
            value = table[args]
        except KeyError:
            value = f(*args)
            table[args] = value
        return value
    memo_f.__name__ = f.__name__
    return memo_f

class NodeType:
    pass

class BitType(NodeType):
    def __getitem__(self, width):
        return bitvector(width)

    def __repr__(self):
        return "bit"

    def __call__(self, node):
        node = as_node(node)
        if node.type == bit:
            return node
        elif isinstance(node.type, BitVectorType):
            if node.type.width == 1:
                return node[0]
            else:
                raise TypeError("Cannot convert bitvector with length not equal to 1 to bit")
        else:
            raise TypeError("Invalid type")

    def cast(self, value):
        return value & 1

class BitVectorType(NodeType):
    def __init__(self, width):
        self.width = width

    def __repr__(self):
        return "bit[%d]" % self.width

    def cast(self, value):
        return value & ((1 << self.width) - 1)

bit = BitType()

@memo
def bitvector(width):
    return BitVectorType(width)

def bits(x):
    if isinstance(x, Iterable):
        return ConcatNode(*map(as_node, x))
    else:
        raise TypeError("Invalid operand type")

def cat(*args):
    return ConcatNode(*map(as_node, args))

def check_type(node, type):
    if node.type != type:
        raise TypeError("Inconsistent types: %s and %s" % (node.type, type))

def check_same_type(node1, node2):
    check_type(node1, node2.type)

class Node:
    def __init__(self, type, name=None):
        self.type = type
        self.name = name

    def __bool__(self):
        raise TypeError("Cannot treat nodes as logical values")

    def __len__(self):
        if isinstance(self.type, BitType):
            return 1
        elif isinstance(self.type, BitVectorType):
            return self.type.width
        else:
            raise TypeError("Unsupported operation")

    def __iter__(self):
        for i in range(len(self)):
            yield self[i]

    def __and__(self, other):
        return BinaryNode('&', self, as_node(other, type=self.type))

    def __rand__(self, other):
        return BinaryNode('&', as_node(other, type=self.type), self)

    def __or__(self, other):
        return BinaryNode('|', self, as_node(other, type=self.type))

    def __ror__(self, other):
        return BinaryNode('|', as_node(other, type=self.type), self)

    def __xor__(self, other):
        return BinaryNode('^', self, as_node(other, type=self.type))

    def __rxor__(self, other):
        return BinaryNode('^', as_node(other, type=self.type), self)

    def __add__(self, other):
        return BinaryNode('+', self, as_node(other, type=self.type))

    def __radd__(self, other):
        return BinaryNode('+', as_node(other, type=self.type), self)

    def __matmul__(self, other):
        return ConcatNode(self, as_node(other))

    def __rmatmul__(self, other):
        return ConcatNode(as_node(other), self)

    def __getitem__(self, index):
        if isinstance(index, int):
            return make_index(self, index)
        elif isinstance(index, slice):
            return make_slice(self, index)
        else:
            raise TypeError("Invalid index type")

class ConcatNode(Node):
    def __init__(self, *operands):
        width = 0
        for operand in operands:
            if isinstance(operand.type, BitVectorType):
                width += operand.type.width
            elif isinstance(operand.type, BitType):
                width += 1
            else:
                raise TypeError("Concatenation operand is not bit or bit vector")

        super().__init__(bit[width])
        self.operands = operands

    def __repr__(self):
        return "(%s)" % ' @ '.join(str(operand) for operand in self.operands)

def wrap_index(i, n):
    return i + n if -n <= i < 0 else i

def make_index(operand, index):
    if not isinstance(operand.type, BitVectorType):
        raise TypeError("Only bit vectors may be indexed")
    if not isinstance(index, int):
        raise TypeError("Only integers may be used as indices")

    index = wrap_index(index, operand.type.width)
    if not 0 <= index < operand.type.width:
        raise TypeError("Index is out of bounds")

    if isinstance(operand, SliceNode):
        operand, index = operand.operand, index + operand.start
    elif isinstance(operand, ConcatNode):
        offset = 0
        for suboperand in operand.operands:
            if suboperand.type == bit:
                if offset == index:
                    return suboperand
                offset += 1
            elif isinstance(suboperand.type, BitVectorType):
                if offset <= index < offset + suboperand.type.width:
                    return make_index(suboperand, index - offset)
                offset += suboperand.type.width
        else:
            assert False

    return IndexNode(operand, index)

class IndexNode(Node):
    def __init__(self, operand, index):
        super().__init__(bit)
        self.operand = operand
        self.index = index

    def __repr__(self):
        return "%s[%d]" % (self.operand, self.index)

def make_slice(operand, index):
    if not isinstance(operand.type, BitVectorType):
        raise TypeError("Only bit vectors may be indexed")
    if index.step is not None:
        raise TypeError("Cannot use a step size when indexing bit vectors")

    start = wrap_index(index.start if index.start is not None else 0, operand.type.width)
    stop = wrap_index(index.stop if index.stop is not None else operand.type.width, operand.type.width)

    if stop <= start:
        raise TypeError("Slice start must be greater than slice stop")
    if stop < 0:
        raise TypeError("Slice start must be nonnegative")
    if start >= operand.type.width:
        raise TypeError("Slice stop must be less than operand width")

    if isinstance(operand, SliceNode):
        operand, start, stop = operand.operand, start + operand.start, stop + operand.start

    return SliceNode(operand, start, stop)

class SliceNode(Node):
    def __init__(self, operand, start, stop):
        super().__init__(bit[stop - start])
        self.operand = operand
        self.start = start
        self.stop = stop

    def __repr__(self):
        return "%s[%d:%d]" % (self.operand, self.start, self.stop)

class WhenNode(Node):
    def __init__(self, cond, then_node, else_node):
        type = then_node.type
        super().__init__(type)
        self.cond = cond
        self.then_node = then_node
        self.else_node = else_node

def when(cond, then_node, else_node):
    check_type(cond, bit)
    check_same_type(then_node, else_node)
    return WhenNode(cond, then_node, else_node)

class RegisterNode(Node):
    def __init__(self, type, init, next, enable):
        super().__init__(type)
        self.init = init
        self.next = next
        self.enable = enable

def register(type, init=None, next=None, enable=None):
    return RegisterNode(type, init, next, enable)

class ConstantNode(Node):
    def __init__(self, type, value):
        super().__init__(type)
        self.value = type.cast(value)

    def __repr__(self):
        return "%d" % self.value

class BinaryNode(Node):
    def __init__(self, op, left, right):
        check_same_type(left, right)
        super().__init__(left.type)
        self.op = op
        self.left = left
        self.right = right

    def __repr__(self):
        return "(%s %s %s)" % (self.left, self.op, self.right)

class InputNode(Node):
    def __repr__(self):
        return "%s" % self.name

def input(type=bit, name=None):
    return InputNode(type, name)

class OutputNode(Node):
    def __init__(self, type, operand):
        super().__init__(type)
        self.operand = operand

    def __repr__(self):
        return "%s(%s)" % (self.name, self.operand)

    def connect(self, node):
        node = as_node(node)
        if self.operand is not None:
            assert ValueError("Output already connected")
        check_type(node, self.type)
        self.operand = node

def output(x=bit):
    if isinstance(x, NodeType):
        operand = None
        type = x
    else:
        operand = as_node(x)
        type = operand.type
    return OutputNode(type, operand)

def as_node(x, type=None):
    if isinstance(x, Node):
        return x
    elif isinstance(x, int) or isinstance(x, bool):
        if type is None:
            n = max(1, x.bit_length())
            type = bit if n == 1 else bit[n]
        return ConstantNode(type, x)
    elif isinstance(x, Iterable):
        return bits(x)
    else:
        raise TypeError("Cannot convert to node")

class InstanceInputNode(Node):
    def __init__(self, type, name, module):
        super().__init__(type)
        self.name = name
        self.module = module

    def connect(self, node):
        self.module.connect(self.name, node)

    def __repr__(self):
        return "%s.%s" % (self.module._name, self.name)

class InstanceOutputNode(Node):
    def __init__(self, type, name, module):
        super().__init__(type)
        self.name = name
        self.module = module

    def __repr__(self):
        return "%s.%s" % (self.module._name, self.name)

class Module:
    def connect(self, name, node):
        node = as_node(node)
        if name in self._inputs:
            pin = self._inputs[name]
        else:
            raise ValueError("No module input with specified name")
        check_same_type(node, pin)
        self._connections[pin] = node

    def __init__(self, **kwargs):
        self._name = None
        self._connections = {}
        for name, node in kwargs.items():
            self.connect(name, node)
        for name, node in self._inputs.items():
            setattr(self, name, InstanceInputNode(node.type, name, self))
        for name, node in self._outputs.items():
            setattr(self, name, InstanceOutputNode(node.type, name, self))

    def __setattr__(self, name, value):
        node = getattr(self, name, None)
        if isinstance(node, InstanceInputNode):
            node.connect(value)
        else:
            return super().__setattr__(name, value)

    def __repr__(self):
        return "%s(%s)" % (type(self).__name__, ', '.join("%s=%s" % (name, value) for name, value in self._connections.items()))

def module(cls):
    inputs = {}
    outputs = {}
    definitions = {}
    namespace = dict(cls.__dict__)
    for name, value in namespace.items():
        if isinstance(value, Module):
            if value._name is None:
                value._name = name
            definitions[name] = value
        elif isinstance(value, Node):
            if value.name is None:
                value.name = name
            if isinstance(value, InputNode):
                inputs[value.name] = value
            elif isinstance(value, OutputNode):
                outputs[value.name] = value
            definitions[name] = value
    namespace['_inputs'] = inputs
    namespace['_outputs'] = outputs
    namespace['_definitions'] = definitions
    for name in definitions:
        del namespace[name]
    return type.__new__(type, cls.__name__, (Module,) + cls.__bases__, namespace)

@module
class Adder1:
    in1 = input(bit)
    in2 = input(bit)
    cin = input(bit)
    p = in1 ^ in2
    out = output(p ^ cin)
    g = in1 & in2
    cout = output(g | p & cin)

def full_adder(x, y, z):
    adder = Adder1(in1=x, in2=y, cin=z)
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

def parity(x):
    if len(x) == 1:
        return x[0]
    else:
        i = len(x) // 2
        return parity(x[:i]) ^ parity(x[i:])

def scan(x, f):
    y = x[0]
    yield y
    for i in range(1, len(x)):
        y = f(y, x[i])
        yield y

def scan2(x, f):
    if len(x) == 1:
        return x
    else:
        i = len(x) // 2
        ys = scan2(x[:i], f)
        return ys @ (f(ys[-1], y) for y in scan2(x[i:], f))

UNVISITED = object()
VISITING = object()

class Visitor:
    def __init__(self):
        self.values = {}

    def default(self, node):
        raise NotImplementedError("Unhandled default case in visitor")

    def __call__(self, node):
        value = self.values.get(node, UNVISITED)
        if value is VISITING:
            raise ValueError("Cyclic node graph")
        elif value is UNVISITED:
            handler = getattr(self, type(node).__name__, self.default)
            self.values[node] = VISITING
            value = handler(node)
            self.values[node] = value
        return value

class DotGenerator(Visitor):
    def __init__(self, show_names=True, show_types=True):
        super().__init__()
        self.show_names = show_names
        self.show_types = show_types
        self.inputs = set()
        self.outputs = set()
        self.vertices = {}
        self.lines = []
        self.next_id = 0

    def line(self, x):
        self.lines.append(x)

    def make_name(self, node, suffix=None, name=None):
        if name is None:
            name = "n%d" % self.next_id
            self.next_id += 1
        path = name + suffix if suffix else name
        self.values[node] = path
        return name, path

    def vertex(self, name, shape, label):
        self.vertices[name] = '%s [ shape = %s, label = "%s" ];' % (name, shape, label)

    def edge(self, from_name, to_name, from_dir='e', to_dir='w'):
        self.line('%s:%s -> %s:%s;' % (from_name, from_dir, to_name, to_dir))

    def header(self, node, label):
        if self.show_names and node.name:
            label = "%s|%s" % (node.name, label)
        if self.show_types and node.type != bit:
            label = "%s|%s" % (label, node.type)
        return label

    def ConstantNode(self, node):
        name, path = self.make_name(node, ':o')
        self.vertex(name, 'record', self.header(node, '<o> %d' % node.value))
        return path

    def BinaryNode(self, node):
        name, path = self.make_name(node, ':o')        
        self.vertex(name, 'record', self.header(node, '{{<i0>|<i1>}|<o> \\%s}' % node.op))
        self.edge(self(node.left), name + ':i0')
        self.edge(self(node.right), name + ':i1')
        return path

    def WhenNode(self, node):
        name, path = self.make_name(node, ':o')
        self.vertex(name, 'record', self.header(node, '{{<c> cond|<t> then|<e> else}|when|<o>}'))
        self.edge(self(node.cond), name + ':c')
        self.edge(self(node.then_node), name + ':t')
        self.edge(self(node.else_node), name + ':e')
        return path

    def RegisterNode(self, node):
        name, path = self.make_name(node, ':o')
        inputs = []
        if node.enable is not None:
            self.edge(self(node.enable), name + ':e')
            inputs.append("<e> enable")
        if node.next is not None:
            self.edge(self(node.next), name + ':n')
            inputs.append("<n> next")
        self.vertex(name, 'record', self.header(node, '{{%s}|register|<o>}' % '|'.join(inputs)))
        return path

    def IndexNode(self, node):
        name, path = self.make_name(node, ':i')
        self.vertex(name, 'record', self.header(node, '<i> [%d]' % node.index))
        self.edge(self(node.operand), name + ':i')
        return path

    def SliceNode(self, node):
        name, path = self.make_name(node, ':i')
        self.vertex(name, 'record', self.header(node, '<i> [%d:%d]' % (node.start, node.stop)))
        self.edge(self(node.operand), name + ':i')
        return path

    def ConcatNode(self, node):
        name, path = self.make_name(node, ':o')
        offset = 0
        sublabels = []
        for i, operand in enumerate(node.operands):
            width = len(operand)
            sublabels.append(offset if width == 1 else "%d:%d" % (offset, offset + width))
            offset += width
        label = '|'.join('<i%d> %s' % (i, sublabel) for i, sublabel in enumerate(sublabels))
        self.vertex(name, 'record', self.header(node, '{{%s}|<o>}' % label))
        for i, operand in enumerate(node.operands):
            self.edge(self(operand), '%s:i%d' % (name, i))
        return path

    def InputNode(self, node):
        name, path = self.make_name(node)
        self.inputs.add(name)
        self.vertex(name, 'rarrow', node.name)
        return path

    def OutputNode(self, node):
        name, path = self.make_name(node)
        self.outputs.add(name)
        self.vertex(name, 'rarrow', node.name)
        if node.operand is not None:
            self.edge(self(node.operand), name)
        return path

    def InstanceOutputNode(self, node):
        name, path = self.make_name(node, name='%s:%s' % (self(node.module), node.name))
        return path

    def Module(self, module):
        name, path = self.make_name(module)
        inputs = '|'.join('<%s> %s' % (input_name, input_name) for input_name in module._inputs)
        outputs = '|'.join('<%s> %s' % (output_name, output_name) for output_name in module._outputs)
        header = module._name + "|" if module._name else ""
        self.vertex(name, 'record', '%s{{%s}|%s|{%s}}' % (header, inputs, type(module).__name__, outputs))
        for input_node, node in module._connections.items():
            self.edge(self(node), '%s:%s' % (name, input_node.name))
        return path

    def default(self, x):
        if isinstance(x, Module):
            return self.Module(x)
        else:
            return super().default(x)

def generate_dot_file(module):
    gen = DotGenerator()
    gen.line('digraph "%s" {' % module.__name__)
    gen.line('graph [ ranksep = 1; rankdir = LR; ]')
    for node in module._outputs.values():
        gen(node)
    for node in module._inputs.values():
        gen(node)
    vertices = dict(gen.vertices)
    gen.line('subgraph inputs { rank = source;')
    for name in gen.inputs:
        gen.line(vertices[name])
        del vertices[name]
    gen.line('}')
    gen.line('subgraph outputs { rank = sink;')
    for name in gen.outputs:
        gen.line(vertices[name])
        del vertices[name]
    gen.line('}')
    gen.line('subgraph nodes {')
    for line in vertices.values():
        gen.line(line)
    gen.line('}')
    gen.line('}')
    return '\n'.join(gen.lines)

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
    y = output(bits(scan2(x, lambda x, y: x ^ y)))

@module
class Counter:
    enable = input(bit)
    count = register(bit[8])
    #count.next = when(enable, count + 1, count)
    count.enable = enable
    count.next = count + 1
    out = output(count)

dot_file = open('example.dot', 'w')
dot_file.write(generate_dot_file(Counter))
