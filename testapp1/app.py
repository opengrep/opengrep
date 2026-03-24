from flask import Flask
from handlers import read_cmd
import runner

app = Flask(__name__)


@app.route("/run")
def run_from_request():
    cmd = read_cmd()
    runner.run_cmd(cmd)
    return "ok"
