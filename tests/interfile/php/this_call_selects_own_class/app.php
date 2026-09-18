<?php
namespace App;

class Store {
    public function start($x) {
        $this->handle($x);
    }

    public function handle($x) {
        // ruleid: this-call-selects-own-class
        sink($x);
    }
}

function go() {
    $s = new Store();
    $s->start($_GET["x"]);
}
