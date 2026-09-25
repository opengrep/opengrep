class Base {
  public function entry() {
    self::run(source());
    static::run_late(source());
  }

  public function run($x) {
    self::helper($x);
  }

  public function run_late($x) {
    static::late($x);
  }

  public function helper($x) {
    // ruleid: hack_self_is_defining_class
    sink($x);
  }

  public function late($x) {
    // ruleid: hack_self_is_defining_class
    sink($x);
  }
}

class Derived extends Base {
  public function helper($x) {
    // ok: hack_self_is_defining_class
    sink($x);
  }

  public function late($x) {
    // ruleid: hack_self_is_defining_class
    sink($x);
  }
}
