s = source()


## sink in comprehension collection

# ruleid: taint
do_sth([x + 1 for x in sink(s)])


## taint in comprehension value

# ruleid: taint
do_sth([sink(s) for x in some_collection])


## taint propagated from collection to result

tainted_col = ["SELECT" + x for x in source()]
# ruleid: taint
sink(tainted_col)

tainted_col2 = ["SELECT" + x for x in s]
# ruleid: taint
sink(tainted_col2)


## taint propagated from collection to result's elements

res2 = [x for x in s]
for x in res2:
  # ruleid: taint
  sink(x)

# ruleid: taint
sink(res2[123])


## taint sanitized in collection

sanitized_col = [x for x in sanitize(s)]
# ok: taint
sink(sanitized_col)


## taint sanitized in expression

sanitized_col = [sanitize(x) for x in s]
# ok: taint
sink(sanitized_col)
