<?php

function rest_only(...$args) {
    // ruleid: php_named_argument_variadic
    sink($args);
}

function rest_after($a, ...$args) {
    // ruleid: php_named_argument_variadic
    sink($args);
}

function rest_beside_bound($x, ...$args) {
    // ok: php_named_argument_variadic
    sink($args);
}

function rest_beside_its_name(...$args) {
    // ruleid: php_named_argument_variadic
    sink($args);
}

function rest_reads_a_field_of_a_named_entry(...$args) {
    // ruleid: php_named_argument_variadic
    sink($args['x']->id);
}

function rest_reads_a_named_key(...$args) {
    // ruleid: php_named_argument_variadic
    sink($args['x']);
}

function rest_reads_a_key_no_argument_provides(...$args) {
    // ok: php_named_argument_variadic
    sink($args['expected']);
}

function rest_calls_a_named_callback(...$cbs) {
    $cbs['f'](source());
}

rest_calls_a_named_callback(f: function ($v) {
    // ruleid: php_named_argument_variadic
    sink($v);
});

rest_calls_a_named_callback(f: function ($v) {
    // ok: php_named_argument_variadic
    sink(1);
});

rest_only(x: source());
rest_reads_a_named_key(x: source());
rest_reads_a_key_no_argument_provides(x: source());
rest_after(1, x: source());
rest_beside_bound(x: source());
rest_beside_its_name(source(), args: 1);

$request = new Request();
$request->id = source();
rest_reads_a_field_of_a_named_entry(x: $request);
