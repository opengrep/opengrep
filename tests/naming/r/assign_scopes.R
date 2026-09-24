x <- 1
f <- function() {
  x <- 2
  x
}
g <- function() {
  x
}
f()
g()
