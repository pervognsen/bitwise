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

    def __bool__(self):
        raise TypeError("Cannot treat nodes as logical values")

class ConstantNode(Node):
    def __init__(self, value):
        self.value = value

class VariableNode(Node):
    def __init__(self, name):
        self.name = name

class UnaryNode(Node):
    def __init__(self, op, node):
        self.op = op
        self.node = node

class BinaryNode(Node):
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

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

operators = {
    '~': lambda x: not x,
    '&': lambda x, y: x and y,
    '|': lambda x, y: x or y,
}

evaluators = {
    ConstantNode: lambda node, state: node.value,
    UnaryNode: lambda node, state: operators[node.op](_evaluate(node.node, state)),
    BinaryNode: lambda node, state: operators[node.op](_evaluate(node.left, state), _evaluate(node.right, state)),
}

def _evaluate(node, state):
    value = state.get(node)
    if value is None:
         value = evaluators[type(node)](node, state)
         state[node] = value
    return value

def evaluate(node, state):
    node = to_node(node)
    state = dict((key, bool(value & 1)) for key, value in state.items())
    return _evaluate(node, state)

x = var('x')
y = x
for i in range(32):
    y = y | ~y

print(evaluate(y, {x: False}))
