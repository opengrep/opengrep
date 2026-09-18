export default class Handler {
  run(x: string) {
    // ok: default-import-binds-class
    sink(x);
  }
}
