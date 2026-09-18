export class Store {
  constructor() {}
  run(x: string) {
    // ruleid: new-binds-imported-class
    sink(x);
  }
}
