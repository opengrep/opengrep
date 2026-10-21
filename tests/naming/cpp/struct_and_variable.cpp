struct lv { int x; } lv;

int get(void) {
  struct lv *p = &lv;
  return p->x;
}
