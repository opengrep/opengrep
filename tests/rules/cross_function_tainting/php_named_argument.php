<?php

function named($partner_id, $ids = null, $other = -1) {
    // ruleid: php_named_argument
    sink($ids);
}

function reordered($partner_id, $ids = null, $other = -1) {
    // ruleid: php_named_argument
    sink($ids);
}

function other_parameter($partner_id, $ids = null, $other = -1) {
    // ok: php_named_argument
    sink($ids);
}

named(1, ids: source());
reordered(other: 2, partner_id: 1, ids: source());
other_parameter(1, other: source());
