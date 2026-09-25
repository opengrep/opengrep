class A {
  public function run($x) {
    $this->helper($x);
  }
  public function other($x) {
    // ok: hack_this_resolves
    sink($x);
  }
  public function helper($x) {
    // ruleid: hack_this_resolves
    sink($x);
  }
}

function main() {
  $a = new A();
  $a->run(source());
}
