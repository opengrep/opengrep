outer <- function(y) {
  helper <- function(x) {
    # ruleid: test-r-nested-assigned-function
    sink(x)
  }
  helper(y)
}

run <- function() {
  outer(source())
}
