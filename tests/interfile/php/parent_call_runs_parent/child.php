<?php
namespace App;

class Child extends Base {
    public function handle($x) {
        parent::handle($x);
    }
}

function go() {
    $c = new Child();
    $c->handle($_GET["x"]);
}
