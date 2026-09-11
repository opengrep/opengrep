<?php
namespace Lib;

class Store {
    public function handle($x) {
        // ok: callable-array-callback
        sink($x);
    }
}
