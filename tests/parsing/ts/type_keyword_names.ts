let a: component.Foo;
let b: component.a.Foo;
let c: component<T>;
let d: component;
let e: unique.Foo;
let f: unique<T>;
let g: unique;
let h: unique symbol;
let i: object.Foo;
let j: object;
const k = (unique: U) => 1;
const l = x as component.Foo;
const m = x as object.Foo;
class C implements component.Bar {}
const n = x as component.a.Foo;
