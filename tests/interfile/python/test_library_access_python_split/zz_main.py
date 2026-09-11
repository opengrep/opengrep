# Field-sensitive taint through Python's getattr library call. A
# source at the bound key fires; a source at a sibling key does not.
# The default-value form evaluates the default eagerly and its taint
# flows into the result via the conditional.
#
# [d.get("k")] is NOT rewritten here because [.get] is overloaded
# across Python's stdlib / third-party libraries and without type
# info we cannot tell a dict receiver apart from a method on some
# other object.

