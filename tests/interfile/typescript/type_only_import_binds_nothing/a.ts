export class Store {
  static run(x: string) {
    // ok: type-only-import-binds-nothing
    sink(x);
  }
}
