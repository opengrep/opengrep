# Test: Direct function-value alias in intrafile mode
# A variable assigned directly to a sink/source-pattern name should propagate
# taint when called through that alias. Currently unsupported; this fixture
# documents the expected behavior pending a fix (gap A from issue #499).

f = sink
# ok: test-direct-alias-sink
f(source())
