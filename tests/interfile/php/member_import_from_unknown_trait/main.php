<?php
use Vendor\Logging\Writer;

trait Local {
    public function handle($x) {
        // ok: member-import-from-unknown-trait
        sink($x);
    }
}

class C {
    use Local, Writer {
        Writer::handle insteadof Local;
    }
}

function run() {
    $c = new C();
    // ruleid: member-import-from-unknown-trait
    sink($c->handle(source()));
}
