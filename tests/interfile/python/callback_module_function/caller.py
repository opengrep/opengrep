import apply
import worker


def go():
    apply.run(worker.handle, source())
