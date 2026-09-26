<?php
trait T1 {
    public function handle($x) {
        // ruleid: trait-insteadof-selects
        sink($x);
    }
}

trait T2 {
    public function handle($x) {
        // ok: trait-insteadof-selects
        sink($x);
    }
}

class C {
    use T1, T2 {
        T1::handle insteadof T2;
    }
}

function run() {
    $c = new C();
    $c->handle(source());
}
