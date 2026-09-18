import { widget } from "./a.js";

function run() {
  const x = widget();   // function call, NOT `new widget()`
  x.render(source());   // must NOT resolve to class widget's render
}
