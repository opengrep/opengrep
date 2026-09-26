<?php
class C {
    use T1, T2 {
        T1::handle insteadof T2;
    }

    public function run() {
        $this->handle(source());
    }
}
