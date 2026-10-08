// A callback inside a method calls another method on the receiver: that
// method's reads of a field the constructor set are reads of the receiver's
// field here, where the constructor's argument reaches it.
const fs = require('fs');
const path = require('path');

class Compiler {
  constructor(historyFilePath) {
    this.history = {};
    this.historyFilePath = historyFilePath;
    this._load();
  }

  setup(app) {
    app.use((req, res, next) => {
      const chunk = path.basename(req.url);
      this._record(chunk);
      next();
    });
  }

  _record(chunk) {
    try {
      // ruleid: lambda_this_method_call
      fs.writeFileSync(this.historyFilePath, JSON.stringify(this.history), 'utf8');
    } catch (e) {}
  }

  _load() {
    try {
      // ruleid: lambda_this_method_call
      this.history = JSON.parse(fs.readFileSync(this.historyFilePath, 'utf8'));
    } catch (e) {}
  }
}

function start(historyFilePath, app) {
  const compiler = new Compiler(historyFilePath);
  compiler.setup(app);
}
