<?php
namespace App;

class Runner {
    public function start($x) {
        static::handle($x);
    }

    public static function handle($x) {
        // ruleid: late-static-binding-calls-class
        sink($x);
    }
}

function go() {
    $r = new Runner();
    $r->start($_GET["x"]);
}
