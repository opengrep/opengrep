// ruleid: flow-function-type-unnamed-rest-parameter
type A = (number, ...Foo) => void;
// ok: flow-function-type-unnamed-rest-parameter
type B = (number, ...x: Foo) => void;
