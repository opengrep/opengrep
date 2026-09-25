<?php
namespace App;

class Store {
    public function handle($x) {
        // ruleid: first-class-callable-callback
        sink($x);
    }
}
