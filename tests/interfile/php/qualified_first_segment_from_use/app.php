<?php
namespace App\Svc;

class Store {
    public static function run($x) {
        // ruleid: qualified-first-segment-from-use
        sink($x);
    }
}
