class Node:
    def __invert__(self):
        return UnaryNode('~', self)

    def __and__(self, other):
        return BinaryNode('&', self, to_node(other))

    def __rand__(self, other):
        return BinaryNode('&', to_node(other), self)

    def __or__(self, other):
        return BinaryNode('|', self, to_node(other))

    def __ror__(self, other):
        return BinaryNode('|', to_node(other), self)

    def __xor__(self, other):
        return BinaryNode('^', self, to_node(other))

    def __rxor__(self, other):
        return BinaryNode('^', to_node(other), self)

    def __bool__(self):
        raise TypeError("Cannot treat nodes as logical values")

class ConstantNode(Node):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return repr(self.value)

class VariableNode(Node):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return str(self.name)

class UnaryNode(Node):
    def __init__(self, op, operand):
        self.op = op
        self.operand = operand

    def __repr__(self):
        return "%s%s" % (self.op, self.operand)

class BinaryNode(Node):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def __repr__(self):
        return "(%s %s %s)" % (self.left, self.op, self.right)

def to_node(x):
    if isinstance(x, Node):
        return x
    elif isinstance(x, bool):
        return ConstantNode(x)
    elif isinstance(x, int):
        return ConstantNode(bool(x & 1))
    else:
        raise TypeError("Type cannot be converted to node")

def var(name):
    return VariableNode(name)

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

class Evaluator(Visitor):
    def __init__(self, environment):
        self.values = {node: bool(value & 1) for node, value in environment.items()}

    def ConstantNode(self, node):
        return node.value

    binary = {
        '&': lambda x, y: x and y,
        '|': lambda x, y: x or y,
        '^': lambda x, y: x ^ y,
    }

    def BinaryNode(self, node):
        return self.binary[node.op](self(node.left), self(node.right))
    
    unary = {
        '~': lambda x: not x,
    }

    def UnaryNode(self, node):
        return self.unary[node.op](self(node.operand))

def evaluate(node, environment):
    return Evaluator(environment)(node)

def evaluate_all(nodes, environment):
    evaluator = Evaluator(environment)
    return [evaluator(node) for node in nodes]

class Copier(Visitor):
    def ConstantNode(self, node):
        return ConstantNode(node.value)
    
    def VariableNode(self, node):
        return VariableNode(node.name)

    def UnaryNode(self, node):
        return UnaryNode(node.op, self(node.operand))

    def BinaryNode(self, node):
        return BinaryNode(node.op, self(node.left), self(node.right))

def copy(node):
    return Copier()(node)
 
class Linearizer(Visitor):
    def __init__(self):
        super().__init__()
        self.counter = 0
        self.inputs = set()
        self.instructions = []

    def make_name(self, node):
        name = "t%d" % self.counter
        self.counter += 1
        return name
    
    def VariableNode(self, node):
        if node.name in self.inputs:
            raise ValueError("Duplicate variable name: %s" % node.name)
        self.inputs.add(node.name)
        name = self.make_name(node)
        self.instructions.append((name, 'VAR', node.name))
        return name
    
    def ConstantNode(self, node):
        name = self.make_name(node)
        self.instructions.append((name, 'CONST', node.value))
        return name
    
    def UnaryNode(self, node):
        operand_name = self(node.operand)
        name = self.make_name(node)
        self.instructions.append((name, 'UNARY', node.op, operand_name))
        return name

    def BinaryNode(self, node):
        left_name = self(node.left)
        right_name = self(node.right)
        name = self.make_name(node)
        self.instructions.append((name, 'BINARY', node.op, left_name, right_name))
        return name

def linearize(nodes):
    linearizer = Linearizer()
    names = [linearizer(node) for node in nodes]
    return names, linearizer.inputs, linearizer.instructions

def compile(nodes, inputs=None, function_name=None):
    ops = {'~': 'not '}
    cases = {
        'VAR': lambda input: input,
        'CONST': lambda value: str(value),
        'UNARY': lambda op, operand: "%s%s" % (ops.get(op, op), operand),
        'BINARY': lambda op, left, right: "%s %s %s" % (left, ops.get(op, op), right),
    }
    outputs, reachable_inputs, instructions = linearize(nodes)
    if inputs:
        assert set(reachable_inputs) <= set(inputs)
    else:
        inputs = reachable_inputs
    statements = []
    for var, tag, *args in instructions:
        statements.append("    %s = %s" % (var, cases[tag](*args)))
    if function_name is None:
        function_name = 'func'
    code = 'def %s(%s):\n%s\n    return %s\n' % (function_name, ', '.join(inputs), '\n'.join(statements), ', '.join(outputs))
    print(code)
    code_locals = {}
    exec(code, globals(), code_locals)
    return code_locals[function_name]
    
class Measurer(Visitor):
    def __init__(self, get_delay):
        super().__init__()
        self.get_delay = get_delay

    def VariableNode(self, node):
        return {node: 0}
    
    def ConstantNode(self, node):
        return {}

    def UnaryNode(self, node):
        delay = self.get_delay(node)
        return {target: delay + max_delay for target, max_delay in self(node.operand).items()}

    def BinaryNode(self, node):
        left_delay, right_delay = self.get_delay(node)
        max_delays = {target: left_delay + max_delay for target, max_delay in self(node.left).items()}
        for target, max_delay in self(node.right).items():
            if target in max_delays:
                max_delays[target] = max(right_delay + max_delay, max_delays[target])
            else:
                max_delays[target] = right_delay + max_delay
        return max_delays

def get_default_delay(node):
    if isinstance(node, BinaryNode):
        return 1, 1
    else:
        return 1

def measure(nodes, get_delay=get_default_delay):
    measurer = Measurer(get_delay)
    return [measurer(node) for node in nodes]

class XorRemover(Copier):
    def BinaryNode(self, node):
        op, left, right = node.op, self(node.left), self(node.right)
        if op == '^':
            return self((left | right) & ~(left & right))
        else:
            return BinaryNode(op, left, right)

def remove_xors(node):
    return XorRemover()(node)

def test():
    x = var('x')
    y = x
    for i in range(32):
        y = y | ~y

    print(evaluate(y, {x: False}))
    print(linearize([y]))

test()

# 3:2 compressor
def full_adder(x, y, z):
    return x ^ y ^ z, (x & y) | (x & z) | (y & z)

x, y, z = var('x'), var('y'), var('z')
print(evaluate(x ^ ~y, {x: True, y: False}))
print(evaluate_all(full_adder(x, y, z), {x: True, y: False, z: True}))

s, c = full_adder(x, y, z)
print(s, c)
s2 = copy(s)
assert s is not s2
assert repr(s) == repr(s2)
print(s2)

a = x & x
assert a.left is a.right
a2 = copy(a)
assert a is not a2
assert repr(a) == repr(a2)
assert a2.left is a2.right

print
print(linearize([~s & 1, c]))
print(measure([s, c]))

my_delays = {
    '~': 1,
    '&': (1, 2),
    '|': (2, 2),
    '^': (3, 3),
}

def get_my_delay(node):
    return my_delays[node.op]

print(measure([x & ~x, ~x & x, x | ~x, x ^ ~x], get_my_delay))

f = compile(full_adder(x, y, z))
print(f(x=True, y=False, z=True))

import dis
dis.dis(f)

e = x ^ (~y & z)
print(linearize([e]))
f = compile([e])
print(f(x=True, y=True, z=False))

e1 = z | ((x & ~z) ^ y)
e2 = remove_xors(e1)
print(e1, "=>", e2)

def make_adder(n):
    x = [var('x%d' % i) for i in range(n)]
    y = [var('y%d' % i) for i in range(n)]
    z, c = n * [0], 0
    for i in range(n):
        z[i], c = full_adder(x[i], y[i], c)
    return z + [c]

def compile_adder(n):
    f = compile(make_adder(n), ['x%d' % i for i in range(n)] + ['y%d' % i for i in range(n)])
    def adder(x, y):
        args = [(x >> i) & 1 for i in range(n)] + [(y >> i) & 1 for i in range(n)]
        return sum(b << i for i, b in enumerate(f(*args)))
    return adder

n = 4
add = compile_adder(n)
for x in range(2**n):
    for y in range(2**n):
        assert x + y == add(x, y)

print(measure(make_adder(n)))
