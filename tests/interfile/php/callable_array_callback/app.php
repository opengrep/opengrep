<?php
namespace App;

class Store {
    public function handle($x) {
        // todoruleid: callable-array-callback
        sink($x);
    }
}
