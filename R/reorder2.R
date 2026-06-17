##' reorder with respect to multiple vectors
##'
##' Like reorder() but takes multiple ordering arguments. The reordering can be with respect to expressions. 
##'
##' @param x The vector to be reordered.
##' @param ... vectors or expressions of vectors to reorder by.
##' @param as One of the following strings and their results
##'
##' "factor" x as a factor with levels according to ...
##' "order" the sorting order of x according to ...
##' "rank" rank of x according to ...
##' "sorted" x sorted according to ...
##' @param decreasing passed to order
##' @param method passed to order
##' @details
##' reorder2 uses order() and passes decreasing and method along. Notice, the default is increasing order. For logicals this means negative comes first (see example).
##'
##' `reorder2()` does not return the scores attribute (which is unlike `reorder()`).
##' 
##' Use rank when assigning value to an ordering column in existing df. 
##' @return A factor, character, or numeric
##' @examples
##' df <- data.frame(char=letters[1:5],num=c(3,2,2,1,3),logic=c(TRUE,FALSE,TRUE,TRUE,FALSE))
##' reorder2(df$char,df$num,df$logic)
##' reorder2(df$char,df$num,df$logic,as="sorted")
##' reorder2(df$char,df$num,df$logic,as="order")
##' reorder2(df$char,df$num,df$logic,as="rank")
##' @export


reorder2 <- function(x, ...,as=c("factor","order","rank","sorted"),decreasing=FALSE,method="auto") {
  ## Logical expressions in ... should prioritized (pushed left) when TRUE, i.e.
  ## sorted as TRUE then FALSE. Because sort(x) when x is logical sorts x as FALSE
  ## then TRUE, those expressions are inverted
  ## args <- lapply(list(...), function(a) if (is.logical(a)) !a else a)

  args <- c(list(...),list(decreasing=decreasing,method=method))
  as <- match.arg(as)

  order.x <- do.call(order, args)
  rank.x <- order(order.x)
  x.ordered <- x[order.x]

  switch(as,
         factor={
           levs <- unique(x.ordered)
           factor(x, levels = levs)
         },
         order=order.x,
         rank=rank.x,
         sorted=x.ordered
         )
}
