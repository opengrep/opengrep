// MATCH:
abstract class Shape {
  @Measure() abstract area(): number;
}
abstract class Plain {
  abstract area(): number;
}
