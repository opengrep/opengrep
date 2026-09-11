<?php
namespace Lib;

class Store {
    public static function run($x) {
        // ok: group-use-binds-two-classes
        sink($x);
    }
}

class Cache {
    public static function put($x) {
        // ok: group-use-binds-two-classes
        sink($x);
    }
}
