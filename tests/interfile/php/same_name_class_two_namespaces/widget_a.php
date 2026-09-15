<?php
// An unrelated class with the same simple name: the same class name and method arity as widget_b.php,
// in another namespace that app.php does not import.  Its presence must not
// suppress the finding through Lib\Widget::process.
namespace Other;

class Widget {
    public function process($x) {
        return strval($x);
    }
}
