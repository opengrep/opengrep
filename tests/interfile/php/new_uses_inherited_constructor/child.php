<?php
namespace App;

class Child extends Base {
    public function emit() {
        // ruleid: new-uses-inherited-constructor
        sink($this->v);
    }
}

function go() {
    $c = new Child($_GET["x"]);
    $c->emit();
}
