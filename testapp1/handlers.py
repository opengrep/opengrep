import flask


def read_cmd():
    return flask.request.args.get("cmd")
