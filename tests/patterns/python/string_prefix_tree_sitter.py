# CPython and the menhir parser reject a comment after a line continuation,
# which the tree-sitter parser accepts, so this target is parsed by tree-sitter.
x =\
    # comment
    0

#ERROR: match
f(r"\d")

f(b"\d")
