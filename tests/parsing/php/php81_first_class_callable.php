<?php
// PHP 8.1: First-class callable syntax

// Function callables
$fn1 = strlen(...);
$fn2 = 'strlen'(...);

// Instance methods
$obj = new Foo();
$fn3 = $obj->method(...);
$fn4 = $obj->$methodStr(...);
$fn5 = ($obj->property)(...);

// Static methods
$fn6 = Foo::method(...);
$fn7 = self::method(...);
$fn8 = parent::method(...);
$fn9 = static::method(...);
$fn10 = $classStr::$methodStr(...);

// Closures and invokables
$fn11 = $closure(...);
$fn12 = $invokableObject(...);

// Array callables
$fn13 = [$obj, 'method'](...);
$fn14 = [Foo::class, 'method'](...);

// Complex expressions
$fn15 = self::{$complex . $expression}(...);

// In class context
class Example {
    public function getCallbacks() {
        return [
            $this->process(...),
            self::handle(...),
            parent::baseMethod(...),
            static::lateBinding(...),
        ];
    }
}
