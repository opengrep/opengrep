class Config {
  static {
    // MATCH:
    loadDefaults();
  }
}

class Other {
  static {
    reset();
  }
}
