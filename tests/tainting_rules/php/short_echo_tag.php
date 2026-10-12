<?php

// '<?=' is 'echo', so what it prints is a sink like any other echo

$name = $_GET['name'];
$safe = htmlspecialchars($name);
?>
<p>
    //ruleid: test-short-echo-tag
    Hello <?= $name ?>
</p>
<p>
    //ruleid: test-short-echo-tag
    <?= 'Hello ', $name ?>
</p>
<p>
    //OK: test-short-echo-tag
    <?= $safe ?>
</p>
//ruleid: test-short-echo-tag
<?= $name;
