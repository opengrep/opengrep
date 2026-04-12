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


## multiple generators: taint flows from each collection

multi = [x + y for x in s for y in other]
# ruleid: taint
sink(multi)

multi2 = [x + y for x in safe for y in s]
# ruleid: taint
sink(multi2)


## multiple generators: taint flows through iteration variable

# ruleid: taint
res_multi = [sink(x) for x in s for y in other]
do_sth(res_multi)


## comprehension with filter: taint flows through

filtered = [x for x in s if x > 0]
# ruleid: taint
sink(filtered)


## multiple generators: taint propagated to result's elements

res_multi2 = [x + y for x in s for y in other]
for item in res_multi2:
  # ruleid: taint
  sink(item)

res_multi3 = [x + y for x in safe for y in s]
for item in res_multi3:
  # ruleid: taint
  sink(item)


## multiple generators: no taint from clean collections

clean_multi = [x + y for x in safe1 for y in safe2]
# ok: taint
sink(clean_multi)
