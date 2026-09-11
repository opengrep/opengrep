<?php
namespace App;

class Store {
    public static function run($x) {
        // ruleid: group-use-binds-two-classes
        sink($x);
    }
}

class Cache {
    public static function put($x) {
        // ruleid: group-use-binds-two-classes
        sink($x);
    }
}
