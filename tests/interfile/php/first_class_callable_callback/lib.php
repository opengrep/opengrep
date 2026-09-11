<?php
namespace Lib;

class Store {
    public function handle($x) {
        // ok: first-class-callable-callback
        sink($x);
    }
}
