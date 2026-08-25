SQL_QUERY = "SELECT COUNT(*) FROM users"
SAFE_VALUE = "safe"


def via_const():
    # ruleid: cp-assign-rhs-const
    query = SQL_QUERY
    return query


def suppressed_by_pattern_not():
    # ok: cp-assign-rhs-not-safe
    value = SAFE_VALUE
    return value


def reported_by_pattern_not():
    # ruleid: cp-assign-rhs-not-safe
    value = "unsafe"
    return value
