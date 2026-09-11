<?php
namespace Lib;

class Widget {
    public function process($x) {
        // ruleid: homonym-class-two-namespaces
        sink($x);
    }
}
