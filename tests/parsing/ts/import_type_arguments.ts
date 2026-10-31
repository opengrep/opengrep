type A = import("./types").Container<string>;
type B = import("./types").Namespace.Item<string, number>;
type C = import("./types")<string>;
type D = import("./types").Container<string>[];
let e: import("./types").Container<string> = f;
