<?php
namespace App;

class Store {
    public static function run($x) {
        // ruleid: use-binds-one-of-two-stores
        sink($x);
    }
}
