summary.compareR <- function(object, ...) {
  UseMethod("summary")
}

print.compareR <- function(x, ...) {
  UseMethod("print")
}

plot.compareR <- function(x, ...) {
  UseMethod("plot")
}
