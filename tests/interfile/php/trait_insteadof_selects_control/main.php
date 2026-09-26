<?php
trait T1 {
    public function handle($x) {
        // ruleid: trait-insteadof-selects-control
        sink($x);
    }
}

class C {
    use T1;
}

function run() {
    $c = new C();
    $c->handle(source());
}
