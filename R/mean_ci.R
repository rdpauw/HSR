##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  mean_ci.R
##  Purpose: calculate mean and CI
##  Author: Robby De Pauw
##  Date: 07/04/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Calculate mean and 95% CI of distribution
#'
#' @param x A continuous variable.
#' @return The mean and 95% CI of \code{x}.
#' @examples
#' \dontrun{
#' mean_ci(x)
#' }
#' @export
mean_ci <- function(x){
  if (TRUE %in% is.na(x)) warning("There is missing data")
  mean_x <- mean(x, na.rm = TRUE)
  names(mean_x) <- "Mean"
  ci_x <- stats::quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)

  res <- c(mean_x, ci_x)
  names(res) <- c("mean", "lower", "upper")
  return(res)

}
