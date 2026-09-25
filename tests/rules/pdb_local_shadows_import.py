import pdb as db


def foo():
    # db is the local assigned below, not the import
    # ok:pdb-remove
    db.set_trace()
    # ok:pdb-remove
    a = "apple"
    #ok:pdb-remove
    db = "the string, not the library"
    #ok:pdb-remove
    pdb = "also a string"
    # ruleid:pdb-remove
    pdb.Pdb.set_trace()
    # db is the string
    # ok:pdb-remove
    db.Pdb.set_trace(...)
