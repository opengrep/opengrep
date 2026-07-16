import json as jsonlib


def query():
    return 1


counter = 0


def shadows():
    # Assignment makes `query` function-local: it must NOT resolve to the
    # module-level `def query` above.
    query = make()
    sink(query)


def uses_global():
    # The `global` directive rebinds the module-level `counter`; the
    # assignment must NOT declare a local.
    global counter
    counter = make()
    sink(counter)


def shadows_import():
    # Ecosystem exception: imports stay flow-insensitive (cf. the pdb.yaml
    # rule in semgrep-rules), so `jsonlib` keeps resolving to the import.
    jsonlib = make()
    sink(jsonlib.dumps)
