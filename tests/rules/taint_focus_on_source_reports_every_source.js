// A rule that reports at the source gives a finding for each source,
// however many reach one sink.
function h(req, db, other) {
  const q =
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.a +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.b +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.c +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.d +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.e +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.f +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.g +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.h +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.i +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.j +
    // ruleid: taint_focus_on_source_reports_every_source
    req.query.k +
    // ok: taint_focus_on_source_reports_every_source
    other.query.z;
  db.query(q);
}
