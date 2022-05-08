#' Print a \code{truncatedFGLM} object
#'
#' Print a summary of truncation point of the fitted \code{truncatedFGLM} model.
#' @details
#' Truncation point estimate of \eqn{\delta} is printed.
#' @param x fitted \code{truncatedFGLM} object
#' @param digits significant digits in printout
#' @param \dots additional print arguments
#' @method print truncatedFGLM
#' @export
print.truncatedFGLM <- function (x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ", deparse(x$call, width.cutoff = 100), "\n\n")
  if (x$scalar.pred) {
    printCoefmat(data.frame(t(x$alpha) , row.names = ""))
  }
  cat("\nOptimal truncation point:", x$trunc.point, "\n")
}
