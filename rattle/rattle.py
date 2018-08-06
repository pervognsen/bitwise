from dis import dis
from collections.abc import Iterable
from functools import total_ordering

def clog2(n):
    assert n > 0
    r = 0
    while n != 1:
        r += 1
        n >>= 1
    if 2**r < n:
        r += 1
    assert 2**r >= n
    return r

@total_ordering
class Wrapper:
    def __init__(self, value):
        self.value = value

    def __hash__(self):
        return object.__hash__(self.value)

    def __eq__(self, other):
        return isinstance(other, Wrapper) and object.__eq__(self.value, other.value)

    def __lt__(self, other):
        return isinstance(other, Wrapper) and id(self.value) < id(other.value)

def wrap(x):
    if isinstance(x, tuple):
        return tuple(wrap(y) for y in x)
    elif isinstance(x, Node):
        return Wrapper(x)
    else:
        return x

def unwrap(x):
    if isinstance(x, tuple):
        return tuple(unwrap(y) for y in x)
    elif isinstance(x, Wrapper):
        return x.value
    else:
        return x

def memo(f):
    table = {}
    def f_memo(*args):
        key = wrap(args)
        try:
            value = table[key]
        except KeyError:
            value = f(*args)
            table[key] = value
        return value
    f_memo.__name__ = f.__name__
    return f_memo

class NodeType:
    pass

class BitType(NodeType):
    def __init__(self):
        super().__init__()
        self.width = 1

    def __getitem__(self, width):
        return bitvector(width)

    def __repr__(self):
        return "bit"

    def __call__(self, node):
        node = as_node(node, type=self)
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

    def __len__(self):
        return self.width

    def cast(self, value):
        return value & ((1 << self.width) - 1)

    def __call__(self, node):
        node = as_node(node, type=self)
        if node.type == self:
            return node
        elif node.type == bit:
            return cat(node, as_node(0, type=bit[self.width - 1]))
        elif isinstance(node.type, BitVectorType):
            if node.type.width < self.width:
                return cat(node, as_node(0, type=bit[self.width - node.type.width]))
            else:
                raise TypeError("Cannot narrow bitvector in cast")
        else:
            raise TypeError("Invalid type")

bit = BitType()

@memo
def bitvector(width):
    return BitVectorType(width)

def bits(x):
    if isinstance(x, Node) and x.type == bit:
        return bit[1](x)
    elif isinstance(x, Iterable):
        return cat(*map(as_node, x))
    else:
        raise TypeError("Invalid operand type")

def cat(*args):
    return make_concat_node(args)

def rep(x, n):
    assert n > 0
    if 2**clog2(n) == n:
        y = x
        m = n
        while m != 1:
            y = y @ y
            m /= 2
        assert len(y) == n * len(x)
        return y
    else:
        return cat(*([x] * n))

def buf(x):
    return make_unary_node('buf', as_node(x))

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

    def __invert__(self):
        return make_unary_node('~', self)

    def __and__(self, other):
        return make_binary_node('&', self, other)

    def __rand__(self, other):
        return make_binary_node('&', other, self)

    def __or__(self, other):
        return make_binary_node('|', self, other)

    def __ror__(self, other):
        return make_binary_node('|', other, self)

    def __xor__(self, other):
        return make_binary_node('^', self, other)

    def __rxor__(self, other):
        return make_binary_node('^', other, self)

    def __add__(self, other):
        return make_binary_node('+', self, other)

    def __mul__(self, other):
        return make_binary_node('*', self, other)

    def __neg__(self):
        return make_unary_node('unary -', self)

    def __radd__(self, other):
        return make_binary_node('+', other, self)

    def __sub__(self, other):
        return make_binary_node('-', self, other)

    def __rsub__(self, other):
        return make_binary_node('-', other, self)

    def __lshift__(self, other):
        return make_shift_node('<<', self, other)

    def __rlshift__(self, other):
        return make_shift_node('<<', other, self)

    def __rshift__(self, other):
        return make_shift_node('>>', self, other)

    def __rrshift__(self, other):
        return make_shift_node('>>', other, self)

    def __matmul__(self, other):
        return cat(self, as_node(other))

    def __rmatmul__(self, other):
        return cat(as_node(other), self)

    def __hash__(self):
        return super().__hash__()

    def __eq__(self, other):
        return make_compare_node('==', self, other)
        
    def __ne__(self, other):
        return make_compare_node('!=', self, other)

    def __le__(self, other):
        return make_compare_node('<=', self, other)

    def __lt__(self, other):
        return make_compare_node('<', self, other)

    def __ge__(self, other):
        return make_compare_node('>=', self, other)

    def __gt__(self, other):
        return make_compare_node('>', self, other)

    def __getitem__(self, index):
        if isinstance(index, int):
            return make_index_node(self, index)
        elif isinstance(index, slice):
            return make_slice_node(self, index)
        else:
            raise TypeError("Invalid index type")

class TraceNode(Node):
    def __init__(self, type, operand, text, base, signed):
        super().__init__(type)
        self.operand = operand
        self.text = text
        self.base = base
        self.signed = signed

def trace(node, text=None, base=10, signed=False):
    assert base in (2, 10, 16)
    node = as_node(node)
    return TraceNode(node.type, node, text, base, signed)

class WireNode(Node):
    def __init__(self, type, operand):
        super().__init__(type)
        self.operand = operand

    def __repr__(self):
        return "wire(%s)" % repr(self.operand)

def wire(x):
    assert isinstance(x, NodeType)
    return WireNode(x, None)

class OperatorNode(Node):
    def __init__(self, type, op, *operands):
        super().__init__(type)
        self.op = op
        self.operands = list(operands)

    def __repr__(self):
        return "(%s %s)" % (self.op, ' '.join(str(operand) for operand in self.operands))

@memo
def make_binary_node_memo(op, left, right):
    check_same_type(left, right)
    return OperatorNode(left.type, op, left, right)

def make_binary_node(op, left, right):
    left, right = as_node(left, type=get_type(right)), as_node(right, type=get_type(left))
    return make_binary_node_memo(op, left, right)

@memo
def make_shift_node_memo(op, left, right):
    return OperatorNode(left.type, op, left, right)

def make_shift_node(op, left, right):
    left, right = as_node(left), as_node(right)
    return make_shift_node_memo(op, left, right)

@memo
def make_unary_node_memo(op, operand):
    return OperatorNode(operand.type, op, operand)

def make_unary_node(op, operand):
    return make_unary_node_memo(op, as_node(operand))

@memo
def make_compare_node_memo(op, left, right):
    check_same_type(left, right)
    return OperatorNode(bit, op, left, right)

def make_compare_node(op, left, right):
    return make_compare_node_memo(op, as_node(left, type=get_type(right)), as_node(right, type=get_type(left)))

@memo
def make_concat_node_memo(operands):
    width = 0
    for operand in operands:
        if isinstance(operand.type, BitVectorType):
            width += operand.type.width
        elif isinstance(operand.type, BitType):
            width += 1
        else:
            raise TypeError("Concatenation operand is not bit or bit vector")
    return OperatorNode(bit[width], '@', *operands)

def make_concat_node(operands):
    new_operands = []
    for operand in operands:
        operand = as_node(operand)
        if operand.type == bit[0]:
            continue
        new_operands.append(operand)
    return make_concat_node_memo(tuple(new_operands))

empty = make_concat_node(())

def wrap_index(i, n):
    return i + n if -n <= i < 0 else i

@memo
def make_index_node_memo(operand, index):
    if isinstance(operand, SliceNode):
        return make_index_node(operand.operand, index + operand.start)
    elif isinstance(operand, OperatorNode) and operand.op == '@':
        offset = 0
        for suboperand in operand.operands:
            if suboperand.type == bit:
                if offset == index:
                    return suboperand
                offset += 1
            elif isinstance(suboperand.type, BitVectorType):
                if offset <= index < offset + suboperand.type.width:
                    return make_index_node(suboperand, index - offset)
                offset += suboperand.type.width
        else:
            assert False

    return IndexNode(operand, index)

def make_index_node(operand, index):
    if not isinstance(operand.type, BitVectorType):
        raise TypeError("Only bit vectors may be indexed")
    if not isinstance(index, int):
        raise TypeError("Only integers may be used as indices")

    index = wrap_index(index, operand.type.width)
    if not 0 <= index < operand.type.width:
        raise TypeError("Index is out of bounds")

    return make_index_node_memo(operand, index)

class IndexNode(Node):
    def __init__(self, operand, index):
        super().__init__(bit)
        self.operand = operand
        self.index = index

    def __repr__(self):
        return "%s[%d]" % (self.operand, self.index)

@memo
def make_memoized_slice_node(operand, start, stop):
    if not isinstance(operand.type, BitVectorType):
        raise TypeError("Only bit vectors may be indexed")
    if stop < start:
        raise TypeError("Slice start must be greater than slice stop")
    if stop < 0:
        raise TypeError("Slice start must be nonnegative")
    if start > operand.type.width:
        raise TypeError("Slice stop must be less than operand width")
    if isinstance(operand, SliceNode):
        return make_memoized_slice_node(operand.operand, start + operand.start, stop + operand.start)
    return SliceNode(operand, start, stop)

def make_slice_node(operand, index):
    if index.step is not None:
        raise TypeError("Cannot use a step size when indexing bit vectors")
    start = wrap_index(index.start if index.start is not None else 0, operand.type.width)
    stop = wrap_index(index.stop if index.stop is not None else operand.type.width, operand.type.width)
    return make_memoized_slice_node(operand, start, stop)

class SliceNode(Node):
    def __init__(self, operand, start, stop):
        super().__init__(bit[stop - start])
        self.operand = operand
        self.start = start
        self.stop = stop

    def __repr__(self):
        return "%s[%d:%d]" % (self.operand, self.start, self.stop)

def get_type(x):
    if isinstance(x, Node):
        return x.type
    else:
        return None

@memo
def when(cond, then_node, else_node):
    if then_node is else_node:
        return then_node

    if isinstance(then_node, tuple):
        assert isinstance(else_node, tuple)
        assert len(then_node) == len(else_node)
        return tuple(when(cond, x, y) for x, y in zip(then_node, else_node))

    cond = as_node(cond, bit)
    then_node, else_node = as_node(then_node, type=get_type(else_node)), as_node(else_node, type=get_type(then_node))
    check_type(cond, bit)
    check_same_type(then_node, else_node)
    return OperatorNode(then_node.type, 'when', cond, then_node, else_node)

class RegisterNode(Node):
    def __init__(self, type, init, next, enable):
        super().__init__(type)
        self.init = init
        self.next = next
        self.enable = enable

    @property
    def next(self):
        return self._next

    @next.setter
    def next(self, node):
        if node is not None:
            node = as_node(node, self.type)
            check_type(node, self.type)

        self._next = node

    @property
    def enable(self):
        return self._enable

    @enable.setter
    def enable(self, node):
        if node is not None:
            node = as_node(node)
            check_type(node, bit)
        
        self._enable = node

def register(type, init=None, next=None, enable=None):
    return RegisterNode(type, init, next, enable=None)

class ConstantNode(Node):
    def __init__(self, type, value):
        super().__init__(type)
        self.value = type.cast(value)

    def __repr__(self):
        return "%d" % self.value

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
        if name in self.inputs:
            pin = self.inputs[name]
        else:
            raise ValueError("No module input with specified name")
        check_same_type(node, pin)
        self._connections[name] = node

    def __init__(self, **kwargs):
        self._name = None
        self._connections = {}
        for name, node in kwargs.items():
            self.connect(name, node)
        for name, node in self.inputs.items():
            setattr(self, name, InstanceInputNode(node.type, name, self))
        for name, node in self.outputs.items():
            setattr(self, name, InstanceOutputNode(node.type, name, self))

    def __setattr__(self, name, value):
        node = getattr(self, name, None)
        if isinstance(node, InstanceInputNode):
            node.connect(value)
        else:
            return super().__setattr__(name, value)

    def __repr__(self):
        return "%s(%s)" % (type(self).__name__, ', '.join("%s=%s" % (name, value) for name, value in self._connections.items()))

import types
from platform import python_implementation

is_pypy = python_implementation() == "PyPy"

def surgery(func):
    code = func.__code__
    co_code = None
    if is_pypy and code.co_code[-4:] == bytes([100, 0, 0, 83]):
        co_code = code.co_code[:-4] + bytes([100, len(code.co_consts), 0, 131, 0, 0, 83])
    elif not is_pypy and code.co_code[-4:] == bytes([100, 0, 83, 0]):
        co_code = code.co_code[:-4] + bytes([100, len(code.co_consts), 131, 0, 83, 0])

    if co_code:
        func.__code__ = types.CodeType(
            code.co_argcount,
            code.co_kwonlyargcount,
            code.co_nlocals,
            code.co_stacksize,
            code.co_flags,
            co_code,
            code.co_consts + (locals,),
            code.co_names,
            code.co_varnames,
            code.co_filename,
            code.co_name,
            code.co_firstlineno,
            code.co_lnotab,
            code.co_freevars,
            code.co_cellvars
        )

    return func

def make_module(module_name, namespace, bases=()):
    inputs = {}
    outputs = {}
    for name, value in namespace.items():
        if name == 'inputs':
            for input_name, input_node in value.items():
                if input_node.name is None:
                    input_node.name = input_name
            inputs.update(value)
        elif name == 'outputs':
            for output_name, output_node in value.items():
                if output_node.name is None:
                    output_node.name = output_name
            outputs.update(value)
        if isinstance(value, Module):
            if value._name is None:
                value._name = name
        elif isinstance(value, Node):
            if value.name is None:
                value.name = name
            if isinstance(value, InputNode):
                inputs[value.name] = value
            elif isinstance(value, OutputNode):
                outputs[value.name] = value
    return type(module_name, (Module,) + bases, {'inputs': inputs, 'outputs': outputs})

def module(x):
    module_name = x.__name__
    if isinstance(x, type):
        bases = x.__bases__
        namespace = dict(x.__dict__)
    elif isinstance(x, types.FunctionType):
        bases = ()
        namespace = dict(surgery(x)())
    return make_module(module_name, namespace, bases)

UNVISITED = object()
VISITING = object()

class Pass:
    def __init__(self):
        self.values = {}

    def default(self, node):
        raise NotImplementedError("Unhandled default case in visitor")

    def get(self, node):
        return self.values.get(wrap(node), UNVISITED)

    def set(self, node, value):
        self.values[wrap(node)] = value
        return value

    def __call__(self, node):
        value = self.get(node)
        if value is VISITING:
            raise ValueError("Cyclic node graph")
        elif value is UNVISITED:
            handler = getattr(self, type(node).__name__, self.default)
            self.set(node, VISITING)
            value = handler(node)
            self.set(node, value)
        return value

def escape(s):
    return ''.join(c if c.isalnum() else '\\' + c for c in s)

operand_labels = {
    'when': ['cond', 'then', 'else'],
}

class DotGenerator(Pass):
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
        self.set(node, path)
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

    def TraceNode(self, node):
        return self(node.operand)
        # name, path = self.make_name(node, ':e')
        # self.vertex(name, 'box', self.header(node, 'trace'))
        # self.edge(self(node.operand) + ':e', name + ':w')
        # return path

    def ConstantNode(self, node):
        name, path = self.make_name(node, ':o')
        self.vertex(name, 'record', self.header(node, '<o> %d' % node.value))
        return path

    def OperatorNode(self, node):
        if node.op == '@':
            return self.concat_node(node)

        name, path = self.make_name(node, ':o')
        input_labels = operand_labels[node.op] if node.op in operand_labels else [''] * len(node.operands)
        inputs = '|'.join('<i%d>%s' % (i, label) for i, label in enumerate(input_labels))
        self.vertex(name, 'record', self.header(node, '{{%s}|<o> %s}' % (inputs, escape(node.op))))
        for i, operand in enumerate(node.operands):
            self.edge(self(operand), name + ':i%d' % i)
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

    def WireNode(self, node):
        name, path = self.make_name(node)
        self.vertex(name, 'circle', '')
        self.edge(self(node.operand), name)
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

    def concat_node(self, node):
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
        inputs = '|'.join('<%s> %s' % (input_name, input_name) for input_name in module.inputs)
        outputs = '|'.join('<%s> %s' % (output_name, output_name) for output_name in module.outputs)
        header = module._name + "|" if module._name else ""
        self.vertex(name, 'record', '%s{{%s}|%s|{%s}}' % (header, inputs, type(module).__name__, outputs))
        for input_name, node in module._connections.items():
            self.edge(self(node), '%s:%s' % (name, input_name))
        return path

    def default(self, x):
        if isinstance(x, Module):
            return self.Module(x)
        else:
            return super().default(x)

def generate_dot_file(module):
    gen = DotGenerator()
    gen.line('digraph "%s" {' % module.__name__)
    gen.line('graph [ ranksep = 2; rankdir = LR; ]')
    for node in module.outputs.values():
        gen(node)
    for node in module.inputs.values():
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

class Transformer(Pass):
    def InputNode(self, node):
        return InputNode(node.type, node.name)

    def RegisterNode(self, node):
        new_node = self.set_node(node, RegisterNode(node.type, node.init, None, None))
        new_node.next = self(node.next) if node.next is not None else None
        new_node.enable = self(node.enable) if node.enable is not None else None
        return new_node

    def set_node(self, node, new_node):
        super().set(node, new_node)
        new_node.name = node.name
        return new_node

    def TraceNode(self, node):
        new_node = self.set_node(node, TraceNode(node.type, None, node.text, node.base, node.signed))
        new_node.operand = self(node.operand)
        return new_node

    def WireNode(self, node):
        new_node = self.set_node(node, WireNode(node.type, None))
        new_node.operand = self(node.operand)
        return new_node

    def OutputNode(self, node):
        new_node = self.set_node(node, OutputNode(node.type, None))
        new_node.operand = self(node.operand)
        return new_node

    def OperatorNode(self, node):
        new_node = self.set_node(node, OperatorNode(node.type, node.op, *((None,) * len(node.operands))))
        for i, operand in enumerate(node.operands):
            new_node.operands[i] = self(operand)
        return new_node

    def IndexNode(self, node):
        new_node = self.set_node(node, IndexNode(None, node.index))
        new_node.operand = self(node.operand)
        return new_node

    def SliceNode(self, node):
        new_node = self.set_node(node, SliceNode(None, node.start, node.stop))
        new_node.operand = self(node.operand)
        return new_node
    
    def ConstantNode(self, node):
        return ConstantNode(node.type, node.value)

    def InstanceInputNode(self, node):
        new_module = self(node.module)
        return getattr(new_module, node.name)

    def InstanceOutputNode(self, node):
        return self.InstanceInputNode(self, node)

    def Module(self, module):
        return type(module)({name: self(node) for name, node in module._connections.items()})
 
    def default(self, x):
        if isinstance(x, Module):
            return self.Module(x)
        else:
            return super().default(x)

class ModuleInliner(Transformer):
    def __init__(self, inputs, prefix=''):
        super().__init__()
        self.inputs = inputs
        self.prefix = prefix

    def set_node(self, node, new_node):
        super().set_node(node, new_node)
        if new_node.name is not None:
            new_node.name = self.prefix + new_node.name
        return new_node

    def InputNode(self, node):
        connected_node = self.inputs.get(node, None)
        if connected_node is None:
            connected_node = super().InputNode(node)
        return connected_node

    def InstanceInputNode(self, node):
        return self(node.operand)

    def InstanceOutputNode(self, node):
        return self(node.module)[node.name]

    def Module(self, module):
        module_class = type(module)
        instance_outputs = {}
        for name, node in module_class.outputs.items():
            instance_outputs[name] = wire(node.type)
        self.set(module, instance_outputs)

        instance_inputs = {}
        for name, node in module._connections.items():
            if name in module_class.inputs:
                instance_inputs[module_class.inputs[name]] = self(node)
        
        module_name = module._name
        if not module_name is not None:
            module_name = module_class.__name__

        prefix = self.prefix + module_name + "_"

        for name, node in inline_module(module_class, instance_inputs, prefix).items():
            instance_outputs[name].operand = node

        return instance_outputs

def inline_module(module, inputs, prefix):
    inliner = ModuleInliner(inputs, prefix)
    return {name: inliner(node).operand for name, node in module.outputs.items()}

def inline_top_module(module, module_name=None):
    if module_name is None:
        module_name = module.__name__ + "Copy"

    inliner = ModuleInliner({})
    inputs = {name: inliner(node) for name, node in module.inputs.items()}
    outputs = {name: inliner(node) for name, node in module.outputs.items()}
    namespace = {}
    namespace.update(inputs)
    namespace.update(outputs)
    return make_module(module_name, namespace, type(module).__bases__)

def copy_module(module, module_name=None):
    if module_name is None:
        module_name = module.__name__ + "Copy"

    copier = Transformer()
    inputs = {name: copier(node) for name, node in module.inputs.items()}
    outputs = {name: copier(node) for name, node in module.outputs.items()}
    namespace = {}
    namespace.update(inputs)
    namespace.update(outputs)
    return make_module(module_name, namespace, type(module).__bases__)

class WireRemover(Transformer):
    def WireNode(self, node):
        return self(node.operand)

def remove_wires(module, module_name=None):
    if module_name is None:
        module_name = module.__name__ + "Copy"

    remover = WireRemover()
    inputs = {name: remover(node) for name, node in module.inputs.items()}
    outputs = {name: remover(node) for name, node in module.outputs.items()}
    namespace = {}
    namespace.update(inputs)
    namespace.update(outputs)
    return make_module(module_name, namespace, type(module).__bases__)

class Linearizer(Pass):
    def __init__(self):
        super().__init__()
        self.instructions = []
        self.counter = 0
        self.registers = {}

    def make_temp(self, node):
        name = "t%d" % self.counter
        self.counter += 1
        return name

    def make_register(self, node):
        return name

    def instruction(self, op, type, *args):
        self.instructions.append((op, type) + tuple(args))

    def TraceNode(self, node):
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'trace', self(node.operand), node.text if node.text else node.operand.name, node.base, node.signed)
        return temp

    def InputNode(self, node):
        assert node.name is not None
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'input', node.name)
        return temp

    def OutputNode(self, node):
        assert node.name is not None
        assert node.operand is not None
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'output', node.name, self(node.operand))
        return temp
    
    def OperatorNode(self, node):
        temp = self.make_temp(node)
        self.instruction(temp, node.type, node.op, *(self(operand) for operand in node.operands))
        return temp

    def IndexNode(self, node):
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'index', self(node.operand), node.index)
        return temp

    def SliceNode(self, node):
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'slice', self(node.operand), node.start, node.stop)
        return temp

    def ConstantNode(self, node):
        temp = self.make_temp(node)
        self.instruction(temp, node.type, 'const', node.value)
        return temp
 
    def WireNode(self, node):
        assert node.operand is not None
        return self(node.operand)

    def RegisterNode(self, node):
        temp = self.make_temp(node)
        self.set(node, temp)
        name = "r%d" % self.counter
        self.counter += 1
        self.instruction(temp, node.type, 'register', name)
        self.registers[name] = (node.type, node.init, self(node.next) if node.next is not None else None)
        return temp

    def submodule_error(self, node):
        assert False, "Submodules must be inlined away before linearization"

    def InstanceInputNode(self, node):
        self.submodule_error(node)

    def InstanceOutputNode(self, node):
        self.submodule_error(node)

def linearize(module):
    module = inline_top_module(module)
    linearizer = Linearizer()
    inputs = {}
    for name, node in module.inputs.items():
        linearizer(node)
        inputs[name] = node.type
    outputs = {}
    for name, node in module.outputs.items():
        result = linearizer(node)
        outputs[name] = (result, node.type)
    return inputs, outputs, linearizer.instructions, linearizer.registers

unary_ops = {'~', 'unary -'}
bitwise_binary_ops = {'&', '|', '^'}
binary_ops = bitwise_binary_ops | {'+', '-', '<<', '>>', '==', '!=', '<=', '<', '>=', '>'}

rattle_op_to_python_op = {
    'unary -': '-',
}

def get_python_op(op):
    return rattle_op_to_python_op.get(op, op)

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

def label(node, name):
#    node.name = str(name)
    return node

def int_to_signed(x, n):
    mask = 1 << (n - 1)
    return (x & (mask - 1)) - (x & mask)

import string

class SimulatorInstance:
    def __iter__(self):
        while True:
            self.update()
            yield self.outputs()
            self.update()
            self.tick()

compile_template = string.Template("""
class $class_name(SimulatorInstance):
    def __init__(self):
        self.trace = {}
$init
        self.reset()

    def reset(self):
$reset

    def update(self, trace=False):
$update

    def tick(self):
$tick

    def outputs(self):
        return bundle($evaluate_outputs)

    @classmethod
    def evaluate(cls, $evaluate_args, trace=False):
        self = cls()
$evaluate_inputs
        self.update(trace=trace)
        if trace:
            for name, value in self.trace.items():
                print(name, "=", value)
        return self.outputs()
""")

def compile(module, class_name=None, trace_all=False):
    if class_name is None:
        class_name = module.__name__

    lines = []
    def line(fmt, *args):
        lines.append(fmt % args)

    masks = set()
    def mask(n):
        s = "bits%d" % n
        if n not in masks:
            line("%s = (1 << %d) - 1", s, n)
            masks.add(n)
        return s

    def join(lines):
        return '\n'.join(' ' * 8 + line for line in lines)

    inputs, outputs, instructions, registers = linearize(module)

    if trace_all:
        line('if trace:')
        for input in inputs:
            line('    self.trace["%s"] = self.%s', input, input)

    types = {}
    for instruction in instructions:
        dest, type, op, *operands = instruction
        types[dest] = type
        if op in unary_ops:
            src = operands[0]
            line('%s = (%s%s) & %s', dest, get_python_op(op), src, mask(type.width))
        elif op in bitwise_binary_ops:
            src1, src2 = operands
            line('%s = %s %s %s', dest, src1, get_python_op(op), src2)
        elif op in binary_ops:
            src1, src2 = operands
            line('%s = (%s %s %s) & %s', dest, src1, get_python_op(op), src2, mask(type.width))
        elif op == '@':
            offset = 0
            terms = []
            for operand in operands:
                width = types[operand].width
                terms.append(operand if offset == 0 else "(%s << %d)" % (operand, offset))
                offset += width
            line('%s = %s', dest, ' | '.join(terms))
        elif op == 'when':
            cond_var, then_var, else_var = operands
            line('%s = %s if %s else %s', dest, then_var, cond_var, else_var)
        elif op == 'index':
            src, index = operands
            line('%s = (%s >> %d) & 1', dest, src, index)
        elif op == 'slice':
            src, start, stop = operands
            line('%s = (%s >> %d) & %s', dest, src, start, mask(stop - start))
        elif op == 'input':
            name = operands[0]
            line('%s = self.%s & %s', dest, name, mask(type.width))
        elif op == 'output':
            name, src = operands
            line('%s = self.%s = %s', dest, name, src)
        elif op == 'register':
            name = operands[0]
            line('%s = self.%s', dest, name)
        elif op == 'const':
            value = operands[0]
            line('%s = %s', dest, value)
        elif op == 'trace':
            value, text, base, signed = operands
            if text is None:
                text = dest
            line('%s = %s', dest, value)
            width = types[value].width
            text += ": "
            if base == 10:
                spec = 'd'
            elif base == 16:
                spec = '0' + str((width + 3) // 4) + 'x'
                text += '0x'
            elif base == 2:
                spec = '0' + str(width) + 'b'
                text += '0b'
            if signed:
                value = 'int_to_signed(%s, %s)' % (value, width)
            line('if trace: print(%s + format(%s, %s))', repr(text), value, repr(spec))
        else:
            assert False

        if trace_all:
            line('if trace: self.trace["%s"] = %s', dest, dest)

    for name, (type, init, next) in registers.items():
        line('self.%s_next = %s' % (name, next))

    if trace_all:
        line('if trace:')
        for output in outputs:
            line('    self.trace["%s"] = self.%s', output, output)

    if registers:
        tick_str = join('self.%s = self.%s_next' % (name, name) for name, (type, init, next) in registers.items())
        reset_str = join('self.%s = self.%s_next = %s' % (name, name, init if init is not None else 0) for name, (type, init, next) in registers.items())
    else:
        tick_str = reset_str = join(['pass'])

    substitutions = {
        'class_name': class_name,
        'init': join('self.%s = 0' % port for port in list(inputs) + list(outputs)),
        'reset': reset_str,
        'update': join(lines),
        'tick': tick_str,
        'evaluate_args': ', '.join(inputs),
        'evaluate_inputs': join("self.%s = %s" % (input, input) for input in inputs),
        'evaluate_outputs': ', '.join("%s=self.%s" % (output, output) for output in outputs),
    }
    code = compile_template.substitute(substitutions)
    code_locals = {}
    exec(code, globals(), code_locals)
    return code_locals[class_name]

def merge_delay(delays, node, delay):
    if node in delays:
        delays[node] = max(delays[node], delay)
    else:
        delays[node] = delay

operator_delays = {
    '~': [1],
    '&': [4/3, 4/3],
    '|': [5/3, 5/3],
    '^': [4, 4],
}

add_delay_per_bit = 5

def fanout_delay(width):
    return clog2(width)

import math

@memo
def get_operator_delay(operator, type, operand):
    if operator in operator_delays:
        return operator_delays[operator][operand]

    width = type.width
    if operator in bitwise_binary_ops:
        return operator_delays[operator][operand]
    elif operator in ('+', '-', '<', '<=', '>=', '>'):
        return math.log2(width) * add_delay_per_bit
    elif operator == 'when':
        delay = 2
        if operand == 0:
            delay += fanout_delay(width)
        return delay
    elif operator in ('@', '<<', '>>'):
        # Assume constant operands for shift, so it represents a fixed wiring pattern
        return 0
    else:
        assert False

class DelayAnalyzer(Pass):
    def TraceNode(self, node):
        return self(node.operand)
    
    def WireNode(self, node):
        return self(node.operand)
    
    def OutputNode(self, node):
        return self(node.operand)
    
    def IndexNode(self, node):
        return self(node.operand)

    def SliceNode(self, node):
        return self(node.operand)

    def InputNode(self, node):
        return {node.name: 0}

    def ConstantNode(self, node):
        return {}
    
    def InstanceInputNode(self, node):
        return self(node.operand)

    def OperatorNode(self, node):
        delays = {}
        for i, operand in enumerate(node.operands):
            operator_delay = get_operator_delay(node.op, node.type, i)
            for input, delay in self(operand).items():
                merge_delay(delays, input, delay + operator_delay)
        return delays

    def InstanceOutputNode(self, node):
        delays = {}
        for name, module_delay in analyze_delay(node.module)[node.name].items():
            for input, delay in self(node.module._connections[name]).items():
                merge_delay(delays, input, delay + module_delay)
        return delays

@memo
def analyze_delay(module):
    analyzer = DelayAnalyzer()
    delays = {}
    for name, output in module.outputs.items():
        delays[name] = analyzer(output)
    return delays

