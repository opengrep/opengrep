<?php
// PHP 8.4: Asymmetric visibility - FAILS TO PARSE
class AsymmetricVisibility {
    public private(set) int $count = 0;

    public function increment(): void {
        $this->count++;
    }
}
