function lambda_form(): void {
  $x = "safe";
  // ok: test-capture-modes-hack
  $f = () ==> sink($x);
  $x = source();
  $f();
}

function anonymous_listed(): void {
  $x = "safe";
  // ok: test-capture-modes-hack
  $f = function() use ($x) { sink($x); };
  $x = source();
  $f();
}

function anonymous_unlisted(): void {
  $x = "safe";
  // ok: test-capture-modes-hack
  $f = function() { sink($x); };
  $x = source();
  $f();
}

function control_before(): void {
  $x = source();
  // ruleid: test-capture-modes-hack
  $f = () ==> sink($x);
  $f();
}
