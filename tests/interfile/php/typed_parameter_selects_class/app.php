<?php
namespace App;

class Store {
    public function run($x) {
        // ruleid: typed-parameter-selects-class
        sink($x);
    }
}
