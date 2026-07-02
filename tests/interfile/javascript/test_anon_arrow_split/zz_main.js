const x = getTainted();
const a = g(x)
// ruleid: test-anon-arrow-taint
sink(a);
