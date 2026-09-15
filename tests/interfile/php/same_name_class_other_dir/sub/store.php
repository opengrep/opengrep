<?php
namespace Sub;

class Store {
    public function save($data) {
        // ruleid: same-name-class-other-dir
        sink($data);
    }
}
