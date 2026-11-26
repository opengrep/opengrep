# PEP 695: type parameter lists (Python 3.12+), with defaults (PEP 696, 3.13+)

def first[T](xs: list[T]) -> T:
    return xs[0]

def bounded[T: int](x: T) -> T:
    return x

def constrained[T: (int, str)](x: T) -> T:
    return x

def with_default[T = int](x: T) -> T:
    return x

def bounded_with_default[T: int = bool](x: T) -> T:
    return x

def variadic[*Ts](*args: *Ts) -> tuple[*Ts]:
    return args

def variadic_with_default[*Ts = *tuple[int, str]]() -> None:
    pass

def param_spec[**P](f: Callable[P, int], *args: P.args, **kwargs: P.kwargs) -> int:
    return f(*args, **kwargs)

def param_spec_with_default[**P = [int, str]]() -> None:
    pass

def mixed[T, *Ts, **P,]() -> None:
    pass

def multiline[
    T,
    U: Callable[[int], str],
]() -> None:
    pass

async def async_generic[T](x: T) -> T:
    return x

@decorator
def decorated[T](x: T) -> T:
    return x

@decorator
async def decorated_async[T](x: T) -> T:
    return x

class Box[T]:
    def get(self) -> T:
        ...

class Derived[T](Base[T]):
    pass

class WithKeywords[T](Base, metaclass=Meta):
    pass

@decorator
class Decorated[T, **P]:
    def method[U](self, x: U) -> T:
        ...

class Regular:
    def ok(self):
        pass

    def generic_method[T](self, x: T) -> T:
        return x

type Alias[T] = list[T]
type Mixed[T: int, *Ts, **P] = Callable[P, tuple[T, *Ts]]
type Multiline[
    T
] = T
if True: type Inline[T] = list[T]
type WithFString[T: f"{a}"] = T
type WithFStringDefault[T = A[f"{a}"]] = T
