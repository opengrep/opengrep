# Cross-function taint within a single file.  Resolving main -> helper
# requires the per-file call graph; in a project scan the graph is
# absolutified by project_root and so must the dataflow lookups (M2).
def helper(x):
    # ruleid: intrafile-cross-function
    sink(x)


def main():
    t = source()
    helper(t)
