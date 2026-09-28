void f(int x) {
  // MATCH:
  switch (x) {
    case 1:
      print("one");
      break;
    case 2:
    case 3:
      var y = x;
      print(y);
      break;
    default:
      print("other");
  }
  if (x == 1) {
    print("one");
  }
}
