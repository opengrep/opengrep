<?php
// A promoted constructor property must be visible as a class field, just
// like the equivalent explicitly declared one.

//ERROR: match
class Explicit {
    public readonly string $secret;

    public function __construct(string $secret) {
        $this->secret = $secret;
    }
}

//ERROR: match
class Promoted {
    public function __construct(public readonly string $secret) {}
}

//ERROR: match
class PromotedAmongOtherParams {
    public function __construct(
        int $port,
        public readonly string $secret,
    ) {}
}

// the modifiers of the promoted property are taken into account, so the
// following must NOT match

class NotReadonly {
    public function __construct(public string $secret) {}
}

class NotPublic {
    public function __construct(private readonly string $secret) {}
}

// a parameter without promotion modifiers is not a field at all

class NotPromoted {
    public function __construct(string $secret) {}
}
