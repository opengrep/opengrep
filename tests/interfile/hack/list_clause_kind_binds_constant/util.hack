namespace App\Util;

function sink(int $x): int {
  return $x;
}

function relay(int $x): int {
  // ruleid: test-hack-list-clause-kind
  return sink($x);
}

function quiet(int $x): int {
  // ok: test-hack-list-clause-kind
  return sink($x);
}
