<?php
// PHP 8.4: Property hooks - FAILS TO PARSE
class PropertyHooks {
    public string $name {
        get {
            return $this->name;
        }
        set {
            $this->name = strtoupper($value);
        }
    }
}
