<?php
namespace App;

class Holder {
    public function __construct(private string $x) {
    }

    public function emit() {
        // ruleid: promoted-property-flows-to-sink
        sink($this->x);
    }
}
