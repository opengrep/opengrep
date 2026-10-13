<?php

// ruleid: test
fopen($_GET['url']);

// ruleid: test
FOPEN($_GET['url']);

// ruleid: test
ReadFile($_GET['url']);

// ok: test
fwrite($_GET['url']);
