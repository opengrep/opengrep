function other_member(C $o): void {
  $o->a = source();
  // ok: member-write-other-member
  sink($o->b);
}

function same_member(C $o): void {
  $o->a = source();
  // ruleid: member-write-other-member
  sink($o->a);
}
