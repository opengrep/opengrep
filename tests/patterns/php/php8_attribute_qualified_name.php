<?php

// namespaced attribute names must survive into the generic AST as a
// qualified name, not be flattened or dropped

//MATCH:
#[App\Attr]
function a(): void {}

//OK: different last component
#[App\Other]
function b(): void {}

//OK: different qualifier
#[Other\Attr]
function c(): void {}

//OK: unqualified
#[Attr]
function d(): void {}
