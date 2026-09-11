<?php
namespace Other;

class Store {
    public function save($data) {
        // ok: homonym-class-other-dir
        sink($data);
    }
}
