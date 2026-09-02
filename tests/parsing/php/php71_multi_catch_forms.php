<?php

// every catch clause takes a list of exception types, not only the first one,
// and the variable may be left out (PHP 8.0)

try {
    f();
} catch (E1 | E2 $e) {
} catch (E3 | E4 $e) {
} catch (E5 $e) {
} catch (E6) {
} catch (E7 | E8) {
} catch (\A\B | \C\D $e) {
} finally {
    g();
}

try {
    h();
} catch (Exception) {
}
