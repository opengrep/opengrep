<?php
namespace App;

class Sinking implements Handler {
    public function handle($x) {
        // ruleid: interface-dispatch-two-implementations
        sink($x);
    }
}
