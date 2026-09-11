<?php
namespace App\Svc;

class Store {
    public static function run($x) {
        // ruleid: qualified-relative-to-namespace
        sink($x);
    }
}
