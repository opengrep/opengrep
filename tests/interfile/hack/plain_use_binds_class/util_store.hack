namespace App\Util;

function sink(int $x): int {
  return $x;
}

class Store {
  public function put(int $x): int {
    // ruleid: test-hack-plain-use
    return sink($x);
  }

  public static function relay(int $x): int {
    // ruleid: test-hack-plain-use
    return sink($x);
  }
}
