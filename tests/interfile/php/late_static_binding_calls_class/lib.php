<?php
namespace Lib;

class Runner {
    public static function handle($x) {
        // ok: late-static-binding-calls-class
        sink($x);
    }
}
