# Decoy: a second, unrelated class with a method of the SAME name.  This
# makes `handle` ambiguous project-wide, so resolving Sub().handle() to
# Base.handle requires real MRO inheritance, not the unique-name fallback.
class Other:
    def handle(self, x):
        pass
