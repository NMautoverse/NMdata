##' Print a data.table as a message, optionally with surrounding text
##'
##' A thin wrapper around `print.data.table` that captures the
##' printed representation of `x` and emits it with `message()`
##' instead of writing to standard output. This is useful for
##' reporting tabular information (e.g. diagnostics or warnings)
##' through R's condition system so it can be suppressed, caught, or
##' redirected like other messages. `before` and `after` allow
##' adding explanatory text around the printed table.
##'
##' @param x a data.table or something to be converted to a
##'     data.table.
##' @param ... passed to print.data.table.
##' @param before optional text (character string) to prepend before
##'     the printed table in the message.
##' @param after optional text (character string) to append after
##'     the printed table in the message.
##' @param sep separator (character string) used to join `before`,
##'     the printed table, and `after`. Default is `"\\n"`.
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
