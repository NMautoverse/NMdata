#' Reorder List Elements by Name
#'
#' Moves specified elements to the front and/or back of a list while 
#' preserving the relative order of all other elements.
#'
#' @param l A named list.
#' @param first A character vector of names to move to the front.
#' @param last A character vector of names to move to the back.
#'
#' @return A list with reordered elements. Names that do not exist 
#' in \code{l} are silently ignored.
#'
#' @export
#'
#' @examples
#' my_list <- list(c = 3, a = 1, d = 4, b = 2)
#' setOrderList(my_list, first = c("a", "b"), last = "c")
setOrderList <- function(l, first = NULL, last = NULL) {
  if (any(first %in% last)) {
    stop("The same name cannot appear in both 'first' and 'last' arguments.")
  }

  nms <- names(l)
  
  prio_first <- intersect(first, nms)
  prio_last <- intersect(last, nms)
  rest <- setdiff(nms, union(first, last))
  
  l[c(prio_first, rest, prio_last)]
}
