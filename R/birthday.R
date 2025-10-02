
#' The Birthday Function
#'
#' @param n The number of students (can be single value or vector)
#'
#' @returns A scatter plot with marginal histogram.
#' @export
#'
#' @examples birthday(5)
birthday = function(n) {
  lfactorial(n) + lchoose(365, n) - n*log(365)
}
