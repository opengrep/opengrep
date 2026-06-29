<?php
function foo($z) {
    switch ($z) {
    // MATCH:
    case (int)$z:
        return 1;
    default:
        return 0;
    }
}
