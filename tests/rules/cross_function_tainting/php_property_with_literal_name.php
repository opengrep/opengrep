<?php
// $o->{'x'} is the property x of the object, so a write to it through a
// parameter reaches the caller's object.
function set_x($o) {
  $o->{'x'} = source();
}
function main() {
  $o = new stdClass();
  set_x($o);
  // ruleid: php_property_with_literal_name
  sink($o->x);
  // ok: php_property_with_literal_name
  sink($o->y);
}
