##' standardize section-denoting strings
##'
##' Always returns capital-case strings without $ or surrounding spaces
##' @param x strings like THETA, $theta, $Theta etc (will all return THETA)
##' @return character string
##' @keywords internal

stringToSection <- function(x){
      section <- cleanSpaces(section)
      section <- sub("^\\$","",section)
      section <- toupper(section)
      section
}
