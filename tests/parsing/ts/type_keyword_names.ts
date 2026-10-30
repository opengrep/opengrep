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
f<opaque[]>();
class A extends B<hook> {}
interface I extends object {}
interface J extends component {}
interface K extends hook.Foo<T> {}
let o: [unique: T];
let p: [component?: T];
let q: (...never: T[]) => void;
let r: (...keyof: T[]) => void;
let s = <out>y;
let t = <out.foo>y;
