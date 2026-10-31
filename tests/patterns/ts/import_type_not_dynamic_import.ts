// MATCH:
const m = import("./types");
type A = import("./types").Item;
type B = typeof import("./types");
