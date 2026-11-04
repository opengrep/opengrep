<?php

//MATCH:
function by_value($x) { return 1; }

//MATCH:
function by_reference(&$x) { return 1; }

//OK:
function two($x, $y) { return 1; }
