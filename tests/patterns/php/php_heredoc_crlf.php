<?php

// a heredoc in a file with Windows line endings: the CRLF before the marker
// comes off whole, while the ones inside the body are part of the value

//ERROR:
$single = <<<EOT
body
EOT;

//ERROR:
$indented = <<<EOT
    body
    EOT;

//ERROR:
$nowdoc = <<<'EOT'
body
EOT;

//ERROR: and the plain string they are equal to
$plain = "body";

// OK: two lines, so not the same string
$multi = <<<EOT
body
body
EOT;
