<?php
namespace App;

class Store {
    use Audit;
}

function go() {
    $s = new Store();
    $s->handle($_GET["x"]);
}
