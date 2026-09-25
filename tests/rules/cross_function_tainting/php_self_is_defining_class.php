<?php

class Base {
  public function run($x) {
    self::helper($x);
  }

  public function run_late($x) {
    static::late($x);
  }

  public function helper($x) {
    // ruleid: php_self_is_defining_class
    sink($x);
  }

  public function late($x) {
    // ruleid: php_self_is_defining_class
    sink($x);
  }
}

class Derived extends Base {
  public function helper($x) {
    // ok: php_self_is_defining_class
    sink($x);
  }

  public function late($x) {
    // ruleid: php_self_is_defining_class
    sink($x);
  }
}

function main() {
  $d = new Derived();
  $d->run(source());
  $d->run_late(source());
}
