<?php
namespace Lib;

class Store {
    public static function run($x) {
        // ok: use-binds-one-of-two-stores
        sink($x);
    }
}
