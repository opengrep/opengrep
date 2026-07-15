from handlers import handle
from hof import run


def poll():
    # Local sharing its name with decoy.query (lemur Family A).
    query = build_filter()
    return run(query, source())


def genuine():
    # Positive control: imported bare function reference stays a callback.
    return run(handle, source())
