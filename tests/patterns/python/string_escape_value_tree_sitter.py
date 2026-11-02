# CPython and the menhir parser reject a comment after a line continuation,
# which the tree-sitter parser accepts, so this target is parsed by tree-sitter.
x =\
    # comment
    0

#ERROR: match
f("AAAA0")

f(r"\x41\101\u0041\U00000041\60")
f("\\x41\\101\\u0041\\U00000041\\60")
