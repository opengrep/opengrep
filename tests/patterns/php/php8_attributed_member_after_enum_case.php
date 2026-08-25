<?php

// the pattern itself declares an enum whose attributed member follows a case,
// so it exercises the menhir parser, which is the only pattern parser and
// cannot fall back to tree-sitter

//ERROR:
enum Level {
    case Low;
    #[Deprecated]
    public function f(): void {}
}

// OK: no attribute on the member
enum Other {
    case Low;
    public function f(): void {}
}
