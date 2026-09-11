<?php
namespace Other;

function helper($x) {
    // ok: unqualified-function-falls-back
    sink($x);
}
