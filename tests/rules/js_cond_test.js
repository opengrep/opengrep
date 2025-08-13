function query_select(query_part) {
    let query_string_select = "SELECT ";
    // ruleid: test
    query_string_select += query_part["distinct"] ? "DISTINCT " : "";
}
