<?php
namespace Sub;

class Store {
    public function save($data) {
        // ruleid: homonym-class-other-dir
        sink($data);
    }
}
