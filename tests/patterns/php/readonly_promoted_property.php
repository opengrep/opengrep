<?php
// PHP 8.1 readonly promoted constructor properties

class Matching {
    //ERROR: match
    public function __construct(public readonly string $secret) {}
}

class ModifiersInAnyOrder {
    //ERROR: match
    public function __construct(readonly public string $secret) {}
}

class AmongOtherParams {
    //ERROR: match
    public function __construct(
        int $port,
        public readonly string $secret,
        string ...$rest,
    ) {}
}

// the readonly modifier must actually be taken into account, so the
// following must NOT match

class NotReadonly {
    public function __construct(public string $secret) {}
}

class NotPublic {
    public function __construct(private readonly string $secret) {}
}

class NotPromoted {
    public readonly string $secret;

    public function __construct(string $secret) {
        $this->secret = $secret;
    }
}
