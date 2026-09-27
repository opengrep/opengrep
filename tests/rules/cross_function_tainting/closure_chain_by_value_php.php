<?php
function chain($handlers) {
  $next = function ($r) { return $r; };
  foreach ($handlers as $h) {
    $next = function ($r) use ($h, $next) { return $h($r, $next); };
  }
  // ruleid: closure_chain_by_value_php
  sink($next(source()));
}
