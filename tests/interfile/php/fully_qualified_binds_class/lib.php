<?php
namespace Lib\Svc;

class Store {
    public static function run($x) {
        // ok: fully-qualified-binds-class
        sink($x);
    }
}
