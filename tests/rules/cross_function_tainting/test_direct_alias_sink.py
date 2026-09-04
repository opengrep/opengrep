# Test: Direct function-value alias in intrafile mode (gap A from issue #499)
# Naming stamps f's uses with the symbolic value `sink`; with
# symbolic_propagation the sink pattern matches the call through the alias.

f = sink
# ruleid: test-direct-alias-sink
f(source())
