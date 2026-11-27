obj <- list()
obj$process <- function(x) {
  # ok: test-r-attribute-assignment
  sink(x)
}

run <- function() {
  process(source())
}
