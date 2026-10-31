class Point {
  #x = 0;
  static isPoint(o) {
    // MATCH:
    return #x in o;
  }
  static isOther(o) {
    return "x" in o;
  }
}
