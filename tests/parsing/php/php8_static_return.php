<?php
// PHP 8.0: Static return type 
class StaticReturn {
    public function create(): static {
        return new static();
    }
}
