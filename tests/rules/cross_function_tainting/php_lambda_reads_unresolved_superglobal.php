<?php
// A superglobal, which naming does not resolve, read by a lambda: the lambda
// captures it.
function arrow() {
  $GLOBALS['x'] = source();
  // ruleid: php_lambda_reads_unresolved_superglobal
  $f = fn() => sink($GLOBALS['x']);
  $f();
}

function closure() {
  $_SESSION['x'] = source();
  $f = function () {
    // ruleid: php_lambda_reads_unresolved_superglobal
    sink($_SESSION['x']);
  };
  $f();
}

function clean() {
  $_SESSION['x'] = "safe";
  $f = function () {
    // ok: php_lambda_reads_unresolved_superglobal
    sink($_SESSION['x']);
  };
  $f();
}
