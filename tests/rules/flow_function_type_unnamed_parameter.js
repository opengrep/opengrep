// ruleid: flow-function-type-unnamed-parameter
type F = (Foo) => void;
// ok: flow-function-type-unnamed-parameter
type G = (Foo: Bar) => void;
