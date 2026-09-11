<?php
namespace App;

class Store {
    public static function run($x) {
        // ruleid: use-alias-binds-class
        sink($x);
    }
}
