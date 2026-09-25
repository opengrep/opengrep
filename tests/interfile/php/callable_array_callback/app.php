<?php
namespace App;

class Store {
    public function handle($x) {
        // ruleid: callable-array-callback
        sink($x);
    }
}
