<?php
// PHP 8.4: Asymmetric visibility
class AsymmetricVisibility {
    // Public read, private write
    public private(set) int $count = 0;

    // Public read, protected write
    public protected(set) string $name = "";

    // Protected read, private write
    protected private(set) int $id = 0;

    public function increment(): void {
        $this->count++;
    }
}
