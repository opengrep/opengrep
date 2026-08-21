from worker_a import Worker


def go():
    w = Worker()
    w.run(source())
