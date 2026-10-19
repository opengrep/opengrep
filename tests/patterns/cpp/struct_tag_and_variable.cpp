struct lv { int x; } lv;

int f() {
  //ERROR: match
  struct lv *lv = h(lv);
}
