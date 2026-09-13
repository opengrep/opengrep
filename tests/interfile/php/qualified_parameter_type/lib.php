<?php
namespace Lib\Svc;

class Store {
    public function run($x) {
        // ok: qualified-parameter-type
        sink($x);
    }
}
