describe("tainted", () => {
  class Tainted {
    entry(x) {
      return this.reaches(x);
    }
    reaches(y) {
      // ruleid: class-in-callback
      sink(y);
    }
  }
  it("passes tainted data", () => {
    new Tainted().entry(source());
  });
});

describe("constant", () => {
  class Constant {
    entry(x) {
      return this.reaches(x);
    }
    reaches(y) {
      // ok: class-in-callback
      sink(y);
    }
  }
  it("passes a constant", () => {
    new Constant().entry("constant");
  });
});
