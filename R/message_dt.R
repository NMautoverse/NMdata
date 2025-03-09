##' @keywords internal
message_dt <- function(dt,as.fun){
    message(paste(capture.output(print(as.fun(dt))),collapse="\n"))
    }
