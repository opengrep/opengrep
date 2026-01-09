<?php

class Foo {
    public string $data {
        get => $this->data;
        set => $this->data = $value;
    }

    public string $name {
        get => $this->name;
        set($val) {
            $this->name = $val;
        }
    }

    public string $withDefault = 'default' {
        get => $this->withDefault;
        set => $this->withDefault = $value;
    }
}

$foo = new Foo();
$foo->data = source();
// ruleid: taint-property-hooks
sink($foo->data);

$bar = new Foo();
$bar->name = source();
// ruleid: taint-property-hooks
sink($bar->name);

$baz = new Foo();
$baz->withDefault = source();
// ruleid: taint-property-hooks
sink($baz->withDefault);
