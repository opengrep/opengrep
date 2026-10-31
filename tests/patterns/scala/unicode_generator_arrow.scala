// `←` is the arrow of a generator, as `<-` is.
object A {
  //ERROR:
  for (x ← xs) sink(x)
  //ERROR:
  for (x <- xs) sink(x)
  //OK:
  for (x ← ys) log(x)
}
