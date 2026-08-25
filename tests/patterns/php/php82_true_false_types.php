<?php

// 'false' used as a type must reach the generic AST as a type, so a rule can
// pick out the functions that return it

//MATCH:
function a(): array|false {}

//OK: 'true', not 'false'
function b(): array|true {}

//OK: a different first member
function c(): string|false {}

//OK: no union
function d(): array {}
