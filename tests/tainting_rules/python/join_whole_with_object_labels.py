def flat(c):
    x = source_a() if c else (source_b(), "")
    # ruleid: join_whole_with_object_labels
    sink_a(x[1])
    # ok: join_whole_with_object_labels
    sink_b(x[1])


def nested(c):
    x = source_a() if c else ((source_b(), ""), "")
    # ruleid: join_whole_with_object_labels
    sink_a(x[0][1])
    # ok: join_whole_with_object_labels
    sink_b(x[0][1])


def own_field(c):
    x = source_a() if c else (source_b(), "")
    # ok: join_whole_with_object_labels
    sink_a(x[0])
    # ruleid: join_whole_with_object_labels
    sink_b(x[0])
