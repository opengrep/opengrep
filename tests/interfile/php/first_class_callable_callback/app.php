<?php
namespace App;

class Store {
    public function handle($x) {
        // todoruleid: first-class-callable-callback
        sink($x);
    }
}
