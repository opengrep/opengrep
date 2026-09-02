<?php

// an intersection type in a parameter is matched as a type, and is not
// confused with a by-reference parameter

//ERROR:
function a(X&Y $p) {}

//ERROR:
class K { public function m(X&Y $p) {} }

// OK: a by-reference parameter of type X
function b(X &$p) {}

// OK: a different intersection
function c(X&Z $p) {}

// OK: a single type
function d(X $p) {}
