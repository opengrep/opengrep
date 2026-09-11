<?php
namespace App;

class Safe implements Handler {
    public function handle($x) {
        return strval($x);
    }
}
