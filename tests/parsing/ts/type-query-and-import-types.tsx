function identity<T,>(value: T): T { return value; }
type Instantiated = ReturnType<typeof identity<string>>;

export function list(items: readonly import("./types").Item[]) {}
export interface List { list(): Promise<readonly import("./types").Item[]>; }
type Imported = import("./types").Container<string>;
const dynamic = import("./runtime");

const view = <div>{identity("visible")}</div>;
afterTypeAnnotations();
