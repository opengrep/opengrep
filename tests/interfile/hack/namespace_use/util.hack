namespace App\Util;

function sink(int $x): int {
  return $x;
}

function relay(int $x): int {
  // ruleid: test-hack-namespace-use
  return sink($x);
}

function quiet(int $x): int {
  // ok: test-hack-namespace-use
  return sink($x);
}

class Box {
  public function put(int $x): int {
    // ruleid: test-hack-namespace-use
    return sink($x);
  }

  public function keep(int $x): int {
    // ok: test-hack-namespace-use
    return sink($x);
  }
}
