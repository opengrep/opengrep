<?php

$config = seed();

function reads_without_global() {
  // A function body does not see the top-level $config: the read is of
  // an undefined local.
  sink($config);
}

$arrow = fn() => sink($config);

$closure = function () use ($config) {
  sink($config);
};

function upper_case_directive() {
  GLOBAL $config;
  sink($config);
}

function declares_global() {
  // The directive creates the global: nothing assigned it before.
  global $created;
  $created = make();
}
