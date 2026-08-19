<?php

// the canonical spelling of the PHP 8.4 attribute is root-qualified, so a
// rule must be able to match '#[\Deprecated]' written exactly that way

//MATCH:
#[\Deprecated]
function a(): void {}

//OK: the pattern does not mention any argument
#[\Deprecated(message: "use a()", since: "8.4")]
function b(): void {}

//OK: not root-qualified
#[Deprecated]
function c(): void {}

//OK: different attribute
#[\Other]
function d(): void {}
