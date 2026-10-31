#!/usr/bin/env -S scala-cli shebang
// After a shebang that does not start a shell, a later line `!#` does not end
// a header.
def f = 1
val s = """
!#
"""
def g = 2
