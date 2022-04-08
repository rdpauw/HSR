##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  dirs.R
##  Purpose: load dirs on Sciensano fileserver
##  Author: Robby De Pauw
##  Date: 04/04/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' dir
#'
#' @param x One of these: "js", "dta"
#' @return The path to the specified directory
dir <- function(x = c("js", "dta")) {
  x <- match.arg(x)
  switch (
    x,
    js = paste0(
      "//sciensano.be/FS/1140_DATA/Health information/",
      "Health status report/REALISATION/Indicator_Sheets_for_Website/",
      "6.Data/Graphs/JS/"
    ),
    dta = paste0(
      "//sciensano.be/FS/1140_DATA/Health information/",
      "Health status report/REALISATION/Indicator_Sheets_for_Website/",
      "6.Data/Data"
    )
  )

}



