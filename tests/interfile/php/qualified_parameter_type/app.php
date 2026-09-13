<?php
namespace App\Svc;

class Store {
    public function run($x) {
        // ruleid: qualified-parameter-type
        sink($x);
    }
}
