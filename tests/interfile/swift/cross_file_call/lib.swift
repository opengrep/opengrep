func leak(_ v: String) {
    // ruleid: cross-file-call-swift
    sink(v)
}
