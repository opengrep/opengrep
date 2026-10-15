<?php

// '<?=' is 'echo': it takes a list, may hold more statements, and may end the
// file without '?>'

?>
<div class="title">
    //MATCH:
    <?= $block->escapeHtml($title) ?>
</div>
<button
    //MATCH: a list
    <?= $block->getAttributesHtml(), $block->escapeHtml($id) ?>>
</button>
<p>
    //MATCH: followed by more statements
    <?= $block->escapeHtml($a); $count++ ?>
</p>
//OK: a plain call, not echoed
<?php $block->escapeHtml($b); ?>
//MATCH: at the end of the file, without '?>'
<?= $block->escapeHtml(__('Thank you.'));
