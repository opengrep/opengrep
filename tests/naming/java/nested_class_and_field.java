class Outer {
  int Inner;

  Inner make() {
    return new Inner();
  }

  class Inner {}

  int get() {
    return Inner;
  }
}
