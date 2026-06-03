##' reorder with respect to multiple vectors
##'
##' Like reorder() but takes multiple ordering arguments. The reordering can be
##' with respect to expressions.
##'
##' @param x The vector to be reordered.
##' @param ... vectors or expressions of vectors to reorder by.
##' @return A factor


reorder2 <- function(x, ...) {
  args <- lapply(list(...), function(a) if (is.logical(a)) !a else a)
  levs <- unique(x[do.call(order, args)])
  factor(x, levels = levs)
}
