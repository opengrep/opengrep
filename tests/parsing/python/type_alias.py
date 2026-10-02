# PEP 695: type alias statements (Python 3.12+)

# Basic type aliases
type IntList = list[int]
type Point = tuple[int, int]
type Name = str

# Complex RHS expressions
type Callback = Callable[[int, str], bool]
type Predicate = Callable[[str], bool]

# Union types (PEP 604 syntax)
type Number = int | float
type Numeric = int | float | complex

# Optional types
type MaybeInt = int | None
type OptionalStr = str | None

# Nested types
type Matrix = list[list[int]]
type DeepNested = dict[str, list[tuple[int, int]]]

# Multiple type aliases in sequence
type A = int
type B = str
type C = float

# Type alias inside a function
def f():
    type LocalAlias = int
    x: LocalAlias = 42

# Type alias inside a class
class MyClass:
    type ClassAlias = str

    def method(self):
        pass

# Type alias after colon (inline in suite)
if True: type InlineAlias = int

# Type alias after semicolon (multiple statements on one line)
x = 1; type AfterSemicolon = int

# Type alias in for/while body (inline suite)
for _ in []: type LoopAlias = int
while False: type WhileAlias = int

# Type alias with string literal RHS (forward reference)
type Forward = "MyClass"

# Type alias with ternary expression RHS
type Conditional = int if True else str

# Type alias immediately after another type alias
type First = int
type Second = str
type Third = First
