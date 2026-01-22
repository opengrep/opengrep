C create(C badArg, D okArg)
{
  // ruleid: taint
  sink(badArg.getData());

  // ok:
  sink(okArg.getData());

  var badNew = new C();
  // ruleid: taint
  sink(badNew.getData());
  
  var goodNew = new D();
  // ok:
  sink(goodNew.getData());

  var bad = C();
  // ruleid: taint
  sink(bad.getData());
  
  var good = D();
  // ok:
  sink(good.getData());

  var good = c();
  // ok:
  sink(good.getData());
}
