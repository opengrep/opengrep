<?php
namespace Lib;

class Store {
    public function run($x) {
        // ok: typed-property-selects-class
        sink($x);
    }
}
