# CPython and the menhir parser reject a comment after a line continuation,
# which the tree-sitter parser accepts, so this target is parsed by tree-sitter.
x =\
    # comment
    0

# ruleid: python2-string-escape-value-tree-sitter
f("\xe9")

# ok: python2-string-escape-value-tree-sitter
f(u"\xe9")
