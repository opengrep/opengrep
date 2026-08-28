/* 'to' precedes 'from' in the call: needs the pending-propagation flush */
void test_to_before_from()
{
  char *str = "tainted";
  char buff[256];
  strcpy(buff, str);
  /* ruleid: taint-propagator */
  sink(buff);
}

/* 'from' precedes 'to' in the call */
void test_from_before_to()
{
  char *str = "tainted";
  char buff[256];
  copy_into(str, buff);
  /* ruleid: taint-propagator */
  sink(buff);
}

void test_no_taint()
{
  char *str = "safe";
  char buff[256];
  strcpy(buff, str);
  /* ok: taint-propagator */
  sink(buff);
}
