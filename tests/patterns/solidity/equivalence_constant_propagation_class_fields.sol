contract A {
  string private f1;
  string private f3 = "abc";
  string private f4;
  string public f5;
  string private immutable f7;
  constructor() { f1 = "abc"; f4 = "abc"; f5 = "abc"; f7 = "abc"; }
  function other() public { f4 = "zzz"; }
  function m() public {
    // ERROR: match
    string memory loc = "abc"; sink(0, loc);
    // ERROR: match
    sink(1, f1);
    // ERROR: match
    sink(3, f3);
    sink(4, f4);
    sink(5, f5);
    // ERROR: match
    sink(7, f7);
  }
}
