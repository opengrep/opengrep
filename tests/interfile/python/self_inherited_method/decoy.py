# Decoy: an unrelated class with methods of the SAME names.  This makes
# `opt` and `helper` ambiguous project-wide, so resolving self.opt(...) and
# self.helper(...) to Base requires a real MRO walk from Child's class up its
# bases, not the unique-method-name fallback.
class Other:
    @staticmethod
    def opt(name):
        pass

    def helper(self, x):
        pass
