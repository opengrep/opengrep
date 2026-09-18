<?php

// Modeled on a type resolver over a syntax tree: one method, six recursive
// calls on different fields of its argument, and a sink inside.
class Walker {
    public function walk($x) {
        if ($x->var !== null) {
            $this->walk($x->var);
        }
        if ($x->expr !== null) {
            $this->walk($x->expr);
        }
        if ($x->left !== null) {
            $this->walk($x->left);
        }
        if ($x->right !== null) {
            $this->walk($x->right);
        }
        if ($x->dim !== null) {
            $this->walk($x->dim);
        }
        if ($x->name !== null) {
            $this->walk($x->name);
        }
        // ruleid: recursive-structure-walk-taint
        sink($x->value);
    }
}
