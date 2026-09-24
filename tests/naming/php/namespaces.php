<?php
namespace App {
  const LIMIT = 3;
  function helper($x) { return $x; }
  function go() { return helper(LIMIT) + \helper(2); }
}
namespace {
  function helper($x) { return $x; }
}
