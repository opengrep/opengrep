<?php
namespace Lib;

class Store {
    public function handle($x) {
        // ok: this-call-selects-own-class
        sink($x);
    }
}
