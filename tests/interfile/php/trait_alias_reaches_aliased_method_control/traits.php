<?php
trait T1 {
    public function handle($x) {
        return $x;
    }
}

trait T2 {
    public function handle($x) {
        // ok: trait-alias-reaches-aliased-method-control
        sink($x);
    }
}
