##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  betaguess.R
##  Purpose: guess the beta-distribution parameters based on CI
##  Author: Robby De Pauw
##  Date: 06/04/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Guess the beta-distribution parameters based on CI
#'
#' @param best The highest probability (see \code{method}).
#' @param lower The lower level of the \code{p} confidence interval.
#' @param upper The upper level of the \code{p} confidence interval.
#' @param p The confidence interval (\code{default}: 95%).
#' @param method Choose mode or mean as best estimate.
#' @return The \code{alpha} and \code{beta} parameters of the beta-distributions.
#' @examples
#' \dontrun{
#' betaguess(best, lower, upper, p = 0.95, method = "mode")
#' }
#' @export

betaguess <- function(best, lower, upper, p = 0.95, method = "mode") {
  if (missing(best))
    stop("'best' is missing")
  if (missing(lower) & missing(upper))
    stop("at least 'lower' or 'upper' must be specified")
  if (!missing(lower))
    if (lower > best)
      stop("'lower' cannot be greater than 'best'")
  if (!missing(upper))
    if (upper < best)
      stop("'upper' cannot be smaller than 'best'")
  if (!missing(lower) & !missing(upper))
    if (lower > upper)
      stop("'lower' cannot be greater than 'upper'")
  f_mode <- function(x, mode, p, target) {
    return(sum((stats::qbeta(p = p, shape1 = x, shape2 = (x * (1 -
                                                          mode) + 2 * mode - 1)/mode) - target)^2))
  }
  f_mode_zero <- function(x, p, target) {
    return((stats::qbeta(p = p, shape1 = 1, shape2 = x) - target)^2)
  }
  f_mode_one <- function(x, p, target) {
    return((stats::qbeta(p = p, shape1 = x, shape2 = 1) - target)^2)
  }
  f_mean <- function(x, mean, p, target) {
    return(sum((stats::qbeta(p = p, shape1 = x, shape2 = (x * (1 -
                                                          mean))/mean) - target)^2))
  }
  if (!missing(lower) & missing(upper)) {
    target <- lower
    p <- 1 - p
  }
  else if (!missing(upper) & missing(lower)) {
    target <- upper
  }
  else if (!missing(upper) & !missing(lower)) {
    target <- c(lower, upper)
    p <- c(0, p) + (1 - p)/2
  }
  if (method == "mode") {
    if (best == 0) {
      a <- 1
      b <- stats::optimize(f_mode_zero, c(0, 1000), p = p, target = target)$minimum
    }
    else if (best == 1) {
      a <- stats::optimize(f_mode_one, c(0, 1000), p = p, target = target)$minimum
      b <- 1
    }
    else {
      a <- stats::optimize(f_mode, c(0, 1000), mode = best, p = p,
                    target = target)$minimum
      b <- (a * (1 - best) + 2 * best - 1)/best
    }
  }
  else if (method == "mean") {
    a <- stats::optimize(f_mean, c(0, 1000), mean = best, p = p,
                  target = target)$minimum
    b <- (a * (1 - best))/best
  }
  out <- list(alpha = a, beta = b)
  class(out) <- "betaExpert"
  return(out)
}
