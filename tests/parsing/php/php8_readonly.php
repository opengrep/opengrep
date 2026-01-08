<?php
// PHP 8.1: Readonly properties - FAILS TO PARSE
class ReadonlyProperty {
    public readonly int $id;

    public function __construct(int $id) {
        $this->id = $id;
    }
}

// PHP 8.2: Readonly classes - FAILS TO PARSE
readonly class ReadonlyClass {
    public int $value;
}
