<?php

// Homonym: same method name as Db::query, but an instance method on an
// unrelated class.  No sink here.  The static call site in main.php
// must NOT resolve to this method via a bare-name fallback.
class Model {
    public function query($q) {
        return "noop: " . $q;
    }
}
