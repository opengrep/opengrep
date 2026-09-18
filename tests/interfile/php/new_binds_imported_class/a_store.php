<?php
namespace Alpha;

class Store {
    private $v;

    public function __construct($x) {
        $this->v = $x;
    }

    public function emit() {
        // ok: new-binds-imported-class
        sink($this->v);
    }
}
