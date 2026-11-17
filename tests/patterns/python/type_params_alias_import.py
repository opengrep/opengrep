from lib import T

type Pair[T] = tuple[T, T]

# ERROR:
type Imported = tuple[T, T]
