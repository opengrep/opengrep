from mod_a import handle


def handle(data):
    # ruleid: rebound-name-definition-after-import
    sink(data)


def run():
    handle(source())
