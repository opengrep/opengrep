<?php

//OK:
function by_value($x) { return 1; }

//MATCH:
function by_reference(&$x) { return 1; }
