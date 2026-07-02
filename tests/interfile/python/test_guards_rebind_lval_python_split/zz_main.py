# ToLval guard rebinding across a call chain. [tagger] guards a
# side-effect assignment to [obj.x] on a parameter. A forwarder passes
# its parameters through to [tagger]; a top-level caller then either
# satisfies or violates the guard, which controls whether [obj.x] ends
# up tainted and thus whether reading it downstream produces a finding.
#
# [forwarder_shift] moves [flag] to a different parameter index than
# [tagger] expects, so the guard's parameter anchor must be rebound
# across the hop rather than carried by position.
#
# Uses an explicit [obj] parameter (BArg ToLval) rather than a [self]
# receiver (BThis ToLval). BThis propagation through method calls is a
# pre-existing issue in opengrep unrelated to rebinding.


# ---------- Direct call: guard resolves at the top level ----------

# ---------- Chain call (same parameter order) ----------

# ---------- Chain call with a shifted parameter index ----------
# [flag] is at index 2 here but index 1 in [tagger]; the guard anchor
# must be rebound across the hop.

