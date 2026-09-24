# Soft keyword disambiguation: 'type' used as an identifier must still parse.

# type as variable name (assignment)
type = 5
type = "hello"

# type() builtin call
x = type(42)
y = type("hello")
z = type([1, 2, 3])

# type as function parameter
def f(type):
    return type

def g(type=int):
    return type

# type as attribute access
class Foo:
    type = int

obj = Foo()
print(obj.type)

# type as keyword argument
def bar(type=None):
    pass
bar(type=int)

# type in expression context
print(type)
t = type
types = [type, int, str]

# type not at statement boundary (in expression)
x = type
result = [type]

# type followed by ( is a call, not alias
type(x)
isinstance(x, type(y))

# type followed by = is assignment to 'type', not alias
type = 5
type = int

# type as dict key (expression, not statement boundary)
d = {"type": int}
d["type"] = str

# type in comprehension
types = [type for type in [int, str]]

# type in lambda
f = lambda type: type

# type after 'else' in inline suite
if False: pass
else: type ElseAlias = int

# Mix of soft keyword and identifier usage in same file
type Alias = int
type = 5
type AnotherAlias = str
print(type)
type YetAnother = float
