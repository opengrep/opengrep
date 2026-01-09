<?php
// PHP 7.1: Multi-catch exception handling

class Exception1 extends Exception {}
class Exception2 extends Exception {}
class Exception3 extends Exception {}

try {
    throw new Exception1("error");
} catch (Exception1 | Exception2 $e) {
    echo "Caught exception 1 or 2: " . $e->getMessage();
} catch (Exception3 | \RuntimeException $e) {
    echo "Caught exception 3 or runtime";
}

// More complex example
try {
    riskyOperation();
} catch (InvalidArgumentException | TypeError | ValueError $e) {
    handleError($e);
}
