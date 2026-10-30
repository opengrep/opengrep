// A function read from an array through an index that the analysis does not
// know, among more functions than a call resolves to, is called as an
// unknown function: its arguments reach its result.
function main(i) {
  const modules = [
    function (x) { return "a"; }, function (x) { return "b"; },
    function (x) { return "c"; }, function (x) { return "d"; },
    function (x) { return "e"; }, function (x) { return "f"; },
    function (x) { return "g"; }, function (x) { return "h"; },
    function (x) { return "i"; }, function (x) { return "j"; },
    function (x) { return "k"; },
  ];
  // ruleid: js_call_through_dynamic_index_is_unknown_call
  sink(modules[i](source()));
  // ok: js_call_through_dynamic_index_is_unknown_call
  sink(modules[i]("safe"));
}
