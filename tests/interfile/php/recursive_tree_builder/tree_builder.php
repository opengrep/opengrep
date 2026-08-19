<?php

// Modeled on a real-world JSON-schema transformer: self-recursive, and each
// branch wraps the recursive result in a fresh container, so the return
// shape nests one level deeper per fixpoint pass (and branch unification
// doubles the node count). Depth widening in effect recording must cut the
// chain: the test both terminates quickly and keeps the finding.
class SchemaTool {
    public function reverseSchema($schema) {
        if (isset($schema['items'])) {
            return [$this->reverseSchema($schema['items'])];
        }
        $result = [];
        foreach ($schema as $key => $value) {
            $result[$key] = $this->reverseSchema($value);
        }
        return $result;
    }

    public function run() {
        $s = source();
        $r = $this->reverseSchema($s);
        // ruleid: recursive-tree-builder-taint
        sink($r['x']);
    }

    public function run_clean() {
        $s = array('a' => 1);
        $r = $this->reverseSchema($s);
        // ok: recursive-tree-builder-taint
        sink($r['x']);
    }
}
