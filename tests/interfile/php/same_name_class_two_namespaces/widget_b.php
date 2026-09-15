<?php
namespace Lib;

class Widget {
    public function process($x) {
        // ruleid: same-name-class-two-namespaces
        sink($x);
    }
}
