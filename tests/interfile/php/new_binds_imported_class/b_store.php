<?php
namespace Beta;

class Store {
    private $w;

    public function __construct($x) {
        $this->w = $x;
    }

    public function emit() {
        // ruleid: new-binds-imported-class
        sink($this->w);
    }
}
