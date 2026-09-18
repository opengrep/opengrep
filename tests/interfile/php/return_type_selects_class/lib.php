<?php
namespace Lib;

class Store {
    public function run($x) {
        // ok: return-type-selects-class
        sink($x);
    }
}
