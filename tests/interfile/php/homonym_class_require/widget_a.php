<?php
// Unrelated homonym: same class name and method arity as widget_b.php,
// but never required by app.php.  Its presence must not suppress the
// finding through widget_b's Widget::process.
class Widget {
    public function process($x) {
        return strval($x);
    }
}
