<?php
namespace Lib;

class Store {
    public function handle($x) {
        // ok: trait-method-inserted-into-class
        sink($x);
    }
}
