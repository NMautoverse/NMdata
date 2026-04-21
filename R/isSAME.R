##' Test whether an initial value is of type SAME
##' @param x A character string to test
##' @keywords internal
##' @examples
##' NMdata:::isSAME("SAME")
##' NMdata:::isSAME(" SAME ")
##' NMdata:::isSAME("SAME(1)")
##' NMdata:::isSAME("SAME (1)")
##' NMdata:::isSAME("  SAME   (  123 ) ")
##' NMdata:::isSAME("SAME()")
##' NMdata:::isSAME("SAME (a)")
##' NMdata:::isSAME("NOT SAME")

isSAME <- function(x) {
  grepl("^\\s*SAME\\s*(\\(\\s*\\d+\\s*\\))?\\s*$", x)
}



##' How many SAME reps?
##' @param x A character string to test
##' @keywords internal
##' @examples
##' NMdata:::NSAME("SAME")
##' NMdata:::NSAME(" SAME ")
##' NMdata:::NSAME("SAME(1)")
##' NMdata:::NSAME("SAME (1)")
##' NMdata:::NSAME("  SAME   (  123 ) ")
##' NMdata:::NSAME("SAME()")
##' NMdata:::NSAME("SAME (a)")
##' NMdata:::NSAME("NOT SAME")


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
