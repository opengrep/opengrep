<?php
namespace Lib;

class S {
    public static function run($x) {
        // ok: use-alias-binds-class
        sink($x);
    }
}
