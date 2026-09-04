<?php

// PHP 7.3: the closing marker may be indented, and may be followed by
// anything rather than only by ';' and a newline

class Mailer
{
    public function body(string $name): string
    {
        return <<<HTML
            <p>Hello {$name}</p>
            HTML;
    }

    public function sql(): string
    {
        return <<<'SQL'
            SELECT * FROM t
            SQL;
    }

    const TEMPLATE = <<<TXT
        a constant
        TXT;
}

$asArgument = trim(<<<TXT
    body
    TXT);

$inArray = ['k' => <<<TXT
    body
    TXT, 'j' => 2];

$noSemicolon = [<<<TXT
    x
    TXT
];

$tabbed = <<<EOT
	body
	EOT;

// coming back from an interpolation leaves the cursor inside a line, where a
// name equal to the marker is body text and not the end of the document

$name = 'x';

$afterInterpolation = <<<EOT
hello {$name} EOT more text
still the body
EOT;

$afterVariable = <<<EOT
hello $name EOT more
still the body
EOT;

$afterOffset = <<<EOT
$arr[0] EOT more
EOT;

$inNowdoc = <<<'EOT'
hello EOT more
EOT;

// a body line that merely starts with the marker's name is not the marker

$notTheEnd = <<<EOT
  EOTX
  EOT;

// a heredoc may itself appear inside an interpolation of another one, and its
// marker is not the outer document's

$nested = <<<OUTER
    x {$f(<<<INNER
    y
    INNER)} z
    more of the outer body
    OUTER;

// and the old spelling, with the marker in the first column, still works

$oldStyle = <<<EOT
line one
line two
EOT;
