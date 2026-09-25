def handler(a):
    # ok: imported_name_rebound_to_value
    sink(a)


def checker(a):
    # ruleid: imported_name_rebound_to_value
    sink(a)
