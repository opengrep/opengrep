const { run, hidden } = require("./a");
function go() {
  run(source());
  hidden(source());
}
module.exports = { go };
