
void performQuery(UserData bad, OtherData good) {

  x = bad.getData();
  // ruleid: taint
  sink(x);


  y = good.getData();
  // ok
  sink(y);

}
