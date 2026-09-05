require_relative 'TestController'
require_relative 'source'
require_relative 'sink'
# Test that hash/array access `obj[:key]` propagates taint correctly.
# `obj[:key]` was misparsed as Call(DotAccess(obj, Op_AREF), [:key])
# instead of ArrayAccess, which broke taint propagation because
# a Call to `[]` has no signature, while ArrayAccess preserves taint.



