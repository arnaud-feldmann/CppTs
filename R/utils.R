

#' Window clone
#'
#' @param x 	a time-series (or other object if not replacing values)
#' @param start the start time of the period of interest.
#' @param end the end time of the period of interest.
#' @param ... ignored
#'
#' @return a time-serie
#' @export
fwindow <- function(x,start=NA_real_,end=NA_real_,...) {
  if (is.null(start)) start <- NA_real_
  if (is.null(end)) end <- NA_real_
  Cpp_window(x, getOption("ts.eps"), start, end)
}
