##' Catch errors, warnings, messages
##'
##' @param expr The expression to catch errors, warnings, messages from
##' @param message Catch messages? Default is `FALSE`.
##' @param warning Catch warnings? Default is `TRUE`.
##' @param error Catch errors? Default is `TRUE`.
##' @examples
##' testfun <- function(x) {
##'     message("Starting function")
##'     if (x == 0) warning("x is zero")
##'     if (x < 0) stop("x is negative")
##'     message("Ending function")
##'     x
##' }
##'
##' ## use the class of the result to test whether anything was
##' ## caught.
##' res1 <- NMdata:::tryCatchAll(testfun(1))  
##' res1
##' inherits(res1,"tryCatchAll")
##' res1b <- NMdata:::tryCatchAll(testfun(1),message=FALSE)  
##' res1b
##' inherits(res1b,"tryCatchAll")
##' 
##' res2 <- NMdata:::tryCatchAll(testfun(0))  
##' res2
##' inherits(res2,"tryCatchAll")
##' 
##' res3 <- NMdata:::tryCatchAll(testfun(-1)) 
##' res3
##' inherits(res3,"tryCatchAll")
##' @keywords internal

tryCatchAll <- function(expr, message = FALSE, warning = TRUE, error = TRUE) {
  warn_obj <- NULL
  msg_obj <- NULL

  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (warning) {
          warn_obj <<- w
          invokeRestart("muffleWarning")
        }
      },
      message = function(m) {
        if (message) {
          msg_obj <<- m
          invokeRestart("muffleMessage")
        }
      }
    ),
    error = function(e) {
      if (error) {
        structure(
          list(type = "error", condition = e, message = if (message) msg_obj else NULL),
          class = "tryCatchAll"
        )
      } else {
        stop(e)  ## re-throw error if error=FALSE
      }
    }
  )

  if (!inherits(result, "tryCatchAll")) {
    if (warning && !is.null(warn_obj)) {
      return(structure(
        list(type = "warning", condition = warn_obj, message = if (message) msg_obj else NULL),
        class = "tryCatchAll"
      ))
    }
    if (message && !is.null(msg_obj)) {
      return(structure(
        list(type = "message", condition = msg_obj),
        class = "tryCatchAll"
      ))
    }
  }

  result
}


