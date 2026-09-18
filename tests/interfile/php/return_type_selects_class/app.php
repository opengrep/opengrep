<?php
namespace App;

class Store {
    public function run($x) {
        // ruleid: return-type-selects-class
        sink($x);
    }
}
