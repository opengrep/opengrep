<?php
class Widget {
    public function process($x) {
        // ruleid: homonym-class-require
        sink($x);
    }
}
