<?php
namespace App;

trait Audit {
    public function handle($x) {
        // ruleid: trait-method-inserted-into-class
        sink($x);
    }
}
