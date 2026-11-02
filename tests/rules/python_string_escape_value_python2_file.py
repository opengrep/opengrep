print "the print statement makes the parser relex this file as Python 2"

# ruleid: python-string-escape-value-python2-file
f("\xe9\u00e9")

# ok: python-string-escape-value-python2-file
f("\\xe9\\u00e9")
