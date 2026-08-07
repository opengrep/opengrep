from sinks import leak_clean, leak_tainted


class Handler:
    def run(self):
        # [a] is bound to source(), [b] to a constant.  Sinking [a] is a real
        # finding; sinking [b] is not.  An off-by-one in the parameter list
        # swaps the two.
        def tainted_first(a, b):
            leak_tainted(a)

        def clean_second(a, b):
            leak_clean(b)

        tainted_first(source(), "safe")
        clean_second(source(), "safe")
