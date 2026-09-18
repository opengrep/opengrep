export class Base {
  handle(x: string) {
    // ok: extends-imported-class
    sink(x);
  }
}
