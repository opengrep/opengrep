<?php
namespace Lib;

class Store {
    public function run($x) {
        // ok: typed-parameter-selects-class
        sink($x);
    }
}
