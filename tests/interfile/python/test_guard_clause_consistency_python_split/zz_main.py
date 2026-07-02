# A guard clause binding the same expression to two distinct constants is
# unsatisfiable: [a == 1 && a == 2] cannot hold for any input, so the sink
# effect is dropped at clause normalisation even though [a] is unknown at
# the call site (three-valued evaluation alone cannot refute it). The same
# check covers length atoms — [len(a) == 1 && len(a) == 2] is the shape a
# cross-arity fused clause takes. The control nest repeats the same atom,
# which stays satisfiable and reports.


# Escape-free distinct strings denote distinct runtime values: refute.
# The Python parser unescapes string contents, so ["\n"] reaches the
# guard as a literal newline -- a runtime value with no backslash. The
# backslash abstention (insurance for parsers that store raw lexed
# contents) does not fire, and the refutation is value-correct.
# Same at evaluation: the substituted ["zzz" == "a\nb"] compares two
# runtime values and folds false, dropping the effect.
