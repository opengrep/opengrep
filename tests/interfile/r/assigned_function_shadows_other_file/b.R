process <- function(x) {
  # ruleid: test-r-assigned-function-shadows
  sink(x)
}

run <- function() {
  process(source())
}
