<?php
namespace App\Svc;

class Store {
    public static function run($x) {
        // ruleid: fully-qualified-binds-class
        sink($x);
    }
}
