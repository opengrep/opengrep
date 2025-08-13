def query_select(query_part):
    query_string_select = "SELECT "
    # ruleid: test
    query_string_select += "DISTINCT " if query_part["distinct"] else ""

