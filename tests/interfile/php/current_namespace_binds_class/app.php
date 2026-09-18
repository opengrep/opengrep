<?php
namespace App;

class Store {
    public static function run($x) {
        // ruleid: current-namespace-binds-class
        sink($x);
    }
}

function go() {
    Store::run($_GET["x"]);
}
