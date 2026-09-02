<?php

// a keyword used as a named-argument label is an ordinary label, in a call and
// in an attribute alike

//ERROR:
f(list: $a, match: $b);

//ERROR:
#[A(list: 1, match: 2)]
function g() {}

// OK: a different label
f(list: $a, switch: $b);

// OK: the same words, but not as labels
f($list, $match);

// OK: a call to something else
implode(separator: ",", array: $items);
