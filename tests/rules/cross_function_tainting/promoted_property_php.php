<?php

class Holder {
    public function __construct(private string $tainted, private string $clean, readonly string $shown, private(set) string $setOnly, public private(set) string $both, protected(set) string $setClean) {
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

    public function emitSetOnly() {
        // ruleid: promoted_property_php
        sink($this->setOnly);
    }

    public function emitBoth() {
        // ruleid: promoted_property_php
        sink($this->both);
    }

    public function emitSetClean() {
        // ok: promoted_property_php
        sink($this->setClean);
    }
}

function run() {
    $h = new Holder(source(), "x", source(), source(), source(), "x");
    $h->emitTainted();
    $h->emitClean();
    $h->emitShown();
    $h->emitSetOnly();
    $h->emitBoth();
    $h->emitSetClean();
}
