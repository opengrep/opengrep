export default interface Foo {
  a: string;
}
// MATCH:
const b = Foo;
