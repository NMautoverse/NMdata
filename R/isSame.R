##' Test whether an initial value is of type SAME
##' A character string to test
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

isSame <- function(x){
  grepl("^\\s*SAME\\s*(\\(\\s*\\d+\\s*\\))?\\s*$", x)
}

