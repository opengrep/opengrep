export class Store {
  run(x: string) {
    // ok: typed-receiver-selects-class
    sink(x);
  }
}
