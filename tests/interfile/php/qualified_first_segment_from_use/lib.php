<?php
namespace Lib\Svc;

class Store {
    public static function run($x) {
        // ok: qualified-first-segment-from-use
        sink($x);
    }
}
