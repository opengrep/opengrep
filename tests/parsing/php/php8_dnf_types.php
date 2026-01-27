<?php
// PHP 8.2: DNF types (Disjunctive Normal Form)
interface A {}
interface B {}
interface C {}

function processDNF((A&B)|C $input): void {
    // DNF type: intersection combined with union
}
