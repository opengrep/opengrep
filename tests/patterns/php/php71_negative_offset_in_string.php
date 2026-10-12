<?php

// PHP 7.1: the offset of an array read inside a string may be negative

//MATCH:
log_line("last: $parts[-1]");

//MATCH: in a heredoc
log_line(<<<EOT
    last: $parts[-1]
    EOT);

//OK: a different offset
log_line("first: $parts[0]");

//OK: a different array
log_line("last: $other[-2]");
