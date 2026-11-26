sink <- function(x) {
  x
}

relay <- function(x) {
  # ruleid: test-r-shared-namespace
  sink(x)
}

quiet <- function(x) {
  # ok: test-r-shared-namespace
  sink(x)
}
