export class Store {
  constructor() {}
  run(x: string) {
    // ok: new-binds-imported-class
    sink(x);
  }
}
