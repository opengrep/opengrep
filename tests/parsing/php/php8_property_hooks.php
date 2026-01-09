<?php
// PHP 8.4: Property hooks
class PropertyHooks {
    private bool $modified = false;

    // With default value and complex body
    public string $foo = 'default value' {
        get {
            if ($this->modified) {
                return $this->foo . ' (modified)';
            }
            return $this->foo;
        }
        set(string $value) {
            $this->foo = strtolower($value);
            $this->modified = true;
        }
    }

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
