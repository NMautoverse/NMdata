##' Test whether an initial value is of type SAME
##' @param x A character string to test
##' @keywords internal
##' @examples
##' isSAME("SAME")
##' isSAME(" SAME ")
##' isSAME("SAME(1)")
##' isSAME("SAME (1)")
##' isSAME("  SAME   (  123 ) ")
##' isSAME("SAME()")
##' isSAME("SAME (a)")
##' isSAME("NOT SAME")

isSAME <- function(x) {
  grepl("^\\s*SAME\\s*(\\(\\s*\\d+\\s*\\))?\\s*$", x)
}



##' How many SAME reps?
##' @param x A character string to test
##' @keywords internal
##' @examples
##' NSAME("SAME")
##' NSAME(" SAME ")
##' NSAME("SAME(1)")
##' NSAME("SAME (1)")
##' NSAME("  SAME   (  123 ) ")
##' NSAME("SAME()")
##' NSAME("SAME (a)")
##' NSAME("NOT SAME")

## NSAME <- function(x){
##   x <- cleanSpaces(x)
##   res <- 0
##   if(grepl("^\\s*SAME\\s*$" ,x)) res <- 1
##   res.try <- suppressWarnings(
##     as.numeric(sub("^\\s*SAME\\s*\\(\\s*([0-9]+)\\s*\\)\\s*", "\\1",x))
##   )
##   if(!is.na(res.try)) res <- res.try
##   res
## }

NSAME <- function(x){
  x <- cleanSpaces(x)
  res <- rep(NA_real_,length(x))
  res[grepl("^\\s*SAME\\s*$" ,x)] <- 1
  res.try <- suppressWarnings(
    as.numeric(sub("^\\s*SAME\\s*\\(\\s*([0-9]+)\\s*\\)\\s*", "\\1",x))
  )
  
  res[!is.na(res.try)] <- res.try[!is.na(res.try)]

  res
}
