function identity<T>(value: T): T { return value; }
type Instantiated = ReturnType<typeof identity<string>>;
type Qualified = typeof import("./types").factory<string>;

export function list(items: readonly import("./types").Item[]) {}
export interface List { list(): Promise<readonly import("./types").Item[]>; }
type Imported = import("./types").Container<string>;
type Nested = import("./types").Namespace.Item;
const dynamic = import("./runtime");
const comparison = (value as import("./types").Item) < limit;

afterTypeAnnotations();
