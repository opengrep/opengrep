<?php

// an ellipsis still covers the members following a case, now that the cases
// and the other members share one list in the grammar

//ERROR:
enum Level {
    case Low;
    #[Deprecated]
    public function f(): void {}
}

// OK: a backed case is not a bare one
enum Backed: string {
    case A = 'a';
    public function g(): void {}
}

// OK: not an enum
class NotAnEnum {
    public function h(): void {}
}
