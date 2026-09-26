<?php
trait T1 {
    public function handle($x) {
        return $x;
    }
}

trait T2 {
    public function handle($x) {
        // ruleid: trait-alias-reaches-aliased-method
        sink($x);
    }
}
