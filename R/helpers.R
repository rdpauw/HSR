#' Recompile given age data to R-compatible age groups
#'
#' @param x Age groups as character string. It is important that age categories start
#' with < X, and end with X+. In-between categories should be defined as X-X.
#' @return Age group as R-compatible factors.
#' @export
getage <-
  function(x) {
    message("Checking the structure of the input file")
    if (!is.character(x)) {
      stop("The input should be a character string")
    }
      age <-
        c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
          "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
          "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
      agei <- c(0, 1, seq(5, 95, 5))


    if(grepl("-", x)) {
      a <- as.numeric(strsplit(x, "-")[[1]])
      g <- c(which(agei == a[1]), which(agei == a[2]+1)-1)

    } else if(grepl("\\+", x)) {
      a <- as.numeric(gsub("\\+", "", x))
      g <- c(which(agei == a[1]), length(age))

    } else if(grepl("<", x)) {
      a <- as.numeric(gsub("<", "", x))
      g <- c(1, which(agei == a[1])-1)
    }
    age[seq(g[1], g[2])]
  }

#' Recompile continuous age data to R-compatible age groups
#'
#' @param x Age groups as continuous variable.
#' @return Age group as R-compatible factors.
#' @export
getage_num <-
  function(x) {
    message("Checking the structure of the input file")
    if (!is.numeric(x)) {
      stop("The input should be numeric")
    }
    age <-
      c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
        "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
    agei <- c(0, 1, seq(5, 95, 5), Inf)

    cut(x, breaks = agei, labels = age, right = FALSE)
  }

#' Calculate midyear population
#'
#' @param x Statbel DF with variable "YEAR".
#' @param year_ref Reference year for which to calculate mid-year population
#' @param year_plus_one Reference year + 1 (!should be available in the dataset)
#' @return mid-population dataframe.
#' @export
midyear <- function(x, year_ref, year_plus_one) {
  if (FALSE %in% (c(year_ref, year_plus_one) %in% x$YEAR)) {
    stop(paste0("Cannot calculate mid-population for: ", year_ref))
  }
  ## filter
  x <- x[x$YEAR %in% c(year_ref, year_plus_one),]
  ## pivot_wider
  x <- pivot_wider(x, names_from = "YEAR", values_from = "POP")
  ## calculate mid_pop
  x$midpop <- as.numeric(unlist((x[,year_ref] + x[,year_plus_one])/2))
  ## only keep result
  x <- x[,!(names(x) %in% c(year_ref, year_plus_one))]
  ## rename result
  names(x)[names(x) == "midpop"] <- as.character(year_ref)
  ## return result
  return(x)
}
