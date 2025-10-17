const getTainted = () => {
  const y = source();
  return y;
};
function g(z)
{const w =z;
  return w}
const x = getTainted();
const a = g(x)
// ruleid: test-anon-arrow-taint
sink(a);
