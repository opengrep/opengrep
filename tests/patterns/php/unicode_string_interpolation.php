<?php

function f()
{
  // a variable whose name starts with a non-ASCII byte, interpolated
  // inside a double-quoted string
  //MATCH:
  echo "pre $Σtotal post";
}
