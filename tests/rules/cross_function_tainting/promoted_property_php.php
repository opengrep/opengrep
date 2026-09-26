<?php

class Holder {
    public function __construct(private string $tainted, private string $clean, readonly string $shown) {
    }

    public function emitTainted() {
        // ruleid: promoted_property_php
        sink($this->tainted);
    }

    public function emitClean() {
        // ok: promoted_property_php
        sink($this->clean);
    }

    public function emitShown() {
        // ruleid: promoted_property_php
        sink($this->shown);
    }
}

function run() {
    $h = new Holder(source(), "x", source());
    $h->emitTainted();
    $h->emitClean();
    $h->emitShown();
}
