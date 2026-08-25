<?php

// named arguments in attributes (PHP 8.0) must reach the generic AST as
// ArgKwd so that patterns can bind them

//MATCH:
#[Route(name: "home")]
function a(): void {}

//OK: a named argument the pattern does not mention
#[Route(name: "away", method: "GET")]
function b(): void {}

//OK: positional, not named
#[Route("/x")]
function c(): void {}

//OK: different attribute
#[Other(name: "home")]
function d(): void {}
