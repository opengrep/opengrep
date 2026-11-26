export * from "./a";

class Sub extends Base {
  run(): void {
    this.f(source());
  }
}
