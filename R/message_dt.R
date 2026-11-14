##' print a data.table
##'
##' @param x a data.table or something to be converted to a
##'     data.table.
##' @param ... passed to print.data.table.
##' @details defaults arguments to print.data.table (in addition to
##'     `x=dt` which cannot be overwritten) are `class=FALSE`,
##'     `print.keys=FALSE`, `row.names=FALSE`.
##' @importFrom utils modifyList
##' @keywords internal

message_dt <- function(x,...,before,after,sep="\n"){

  if(missing(before)) before <- NULL
  if(missing(after)) after <- NULL

    if(!is.data.table(x)){
        x <- as.data.table(x)
    }

    dots <- list(...)
    defaults <- list(
        class=FALSE,
        print.keys=FALSE,
        row.names=FALSE
    )
    ## prioritize dots
    dots <- modifyList(defaults,dots)

msg <- 
    paste(capture.output(
        do.call(print,c(list(x=x),dots))
    ),collapse="\n")

  if(!is.null(before)) msg <- paste(before,msg,sep=sep)
  if(!is.null(after)) msg <- paste(msg,after,sep=sep)

message(msg)

    invisible(x)
}
