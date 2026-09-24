class A {
  #f1;
  #f3 = "abc";
  f5;
  f8 = "abc";
  constructor() { this.#f1 = "abc"; this.f5 = "abc"; this.f6 = "abc"; }
  m() {
    // ERROR: match
    const loc = "abc"; sink(0, loc);
    // ERROR: match
    sink(1, this.#f1);
    // ERROR: match
    sink(3, this.#f3);
    sink(5, this.f5);
    sink(6, this.f6);
    sink(8, this.f8);
  }
}
