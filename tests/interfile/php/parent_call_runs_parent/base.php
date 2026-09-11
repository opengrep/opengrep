<?php
namespace App;

class Base {
    public function handle($x) {
        // ruleid: parent-call-runs-parent
        sink($x);
    }
}
