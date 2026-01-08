<?php
// PHP 8.4: Property hooks
class PropertyHooks {
    // Block syntax
    public string $name {
        get {
            return $this->name;
        }
        set {
            $this->name = strtoupper($value);
        }
    }

    // Arrow syntax (simpler)
    public string $firstName {
        get => $this->firstName;
        set => $this->firstName = trim($value);
    }

    // With explicit set parameter
    public int $age {
        get => $this->age;
        set($val) {
            $this->age = $val;
        }
    }
}
