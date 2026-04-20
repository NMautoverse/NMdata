##' Test whether an initial value is of type SAME
##' @param x A character string to test
##' @keywords internal
##' @examples
##' isSame("SAME")
##' isSame(" SAME ")
##' isSame("SAME(1)")
##' isSame("SAME (1)")
##' isSame("  SAME   (  123 ) ")
##' isSame("SAME()")
##' isSame("SAME (a)")
##' isSame("NOT SAME")

isSAME <- function(x) {
  grepl("^\\s*SAME\\s*(\\(\\s*\\d+\\s*\\))?\\s*$", x)
}

NSAME <- function(x){
  x <- cleanSpaces(x)
  res <- 0
  if(grepl("^\\s*SAME\\s*$" ,x)) res <- 1
  if(grepl("^\\s*SAME\\s*\\(.*\\)\\s*$" ,x)) {
    res <- as.numeric(sub("^\\s*SAME\\(\\s*([0-9]+)\\s*\\)\\s*", "\\1",x))
  }
  res
}
