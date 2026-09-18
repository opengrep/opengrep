<?php
namespace Lib;

class Holder {
    public function __construct(private string $x) {
    }

    public function emit() {
        // ok: promoted-property-flows-to-sink
        sink($this->x);
    }
}
