##' @keywords internal
message_dt <- function(dt,as.fun){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    message(paste(capture.output(print(as.fun(dt))),collapse="\n"))
}
