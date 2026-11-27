namespace App\Other;

function sink(int $x): int {
  return $x;
}

class Store {
  public function put(int $x): int {
    // ok: test-hack-plain-use
    return sink($x);
  }

  public static function relay(int $x): int {
    // ok: test-hack-plain-use
    return sink($x);
  }
}
