<?php
class C {
    use T1, T2 {
        T1::handle insteadof T2;
        T2::handle as protected quiet;
    }

    public function run() {
        $this->quiet(source());
    }
}
