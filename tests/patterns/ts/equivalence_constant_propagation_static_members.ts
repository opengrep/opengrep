class A {
  static #s;
  static #u = "abc";
  static constructor() { this.#s = "abc"; }
  static m() {
    sink(1, this.#s);
    // ERROR: match
    sink(2, this.#u);
  }
}
