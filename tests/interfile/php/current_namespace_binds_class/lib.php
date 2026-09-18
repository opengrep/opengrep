<?php
namespace Lib;

class Store {
    public static function run($x) {
        // ok: current-namespace-binds-class
        sink($x);
    }
}
