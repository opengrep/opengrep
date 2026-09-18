<?php
namespace Svc;

class Store {
    public static function run($x) {
        // ok: qualified-relative-to-namespace
        sink($x);
    }
}
