<?php

// a heredoc or a nowdoc is a constant expression, so it may initialise a
// constant, a property, a parameter, a static variable or an enum case

const T = <<<EOT
plain
EOT;

const N = <<<'EOT'
nowdoc
EOT;

class K
{
    const E = <<<EOT
in a class
EOT;

    public $prop = <<<'EOT'
a property default
EOT;

    public function m($x = <<<EOT
a parameter default
EOT
    ) {}

    public function n()
    {
        static $s = <<<EOT
a static variable
EOT;
        return $s;
    }
}

enum Backed: string
{
    case Bar = <<<BAR
an enum case
BAR;
}
