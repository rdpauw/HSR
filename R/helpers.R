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
  x <- tidyr::pivot_wider(x, names_from = "YEAR", values_from = "POP")
  ## calculate mid_pop
  x$midpop <- as.numeric(unlist((x[,year_ref] + x[,year_plus_one])/2))
  ## only keep result
  x <- x[,!(names(x) %in% c(year_ref, year_plus_one))]
  ## rename result
  names(x)[names(x) == "midpop"] <- as.character(year_ref)
  ## return result
  return(x)
}

#' Calculate population distribution
#'
#' @param x A database containing the variables SEX, AGE, AND REGION.
#' @param year_ref Reference year to use for the calculations.
#' @param name_pop_var Character variable with name of population variable.
#' @param by Character variable of combination on which the distribution should be calculated.
#' @param name_rate_var Name of the variable to transform in age-adjusted estimates
#' @return Dataframe with population distribution and age-adjusted estimates.
#' @export
pop_distribution <- function(x, year_ref, name_pop_var = "POP",
                             by = c("REGION", "SEX"), name_rate_var = "rate") {

  ## filter
  x <- x[x$YEAR %in% c(year_ref),] ## filter YEAR

  ## create new variable to save result
  x$popdens <- 0

  ## over which combinations?
  x_combn <- unique(x[by])

  for (comn_i in 1:(dim(x_combn)[1])) {
    ## check if TRUE
    id <- apply(x[,by], 1, function(x) !(FALSE %in% (x == x_combn[comn_i,])))

    n_tot <- sum(x[id, name_pop_var])
    x[id, "popdens"] <- (x[id, name_pop_var]) / n_tot
  }

  ## calculate age-adjusted rate
  x$age_adjusted_rate <- x[name_rate_var] * x["popdens"]
  x$age_adjusted_rate <- unlist(x$age_adjusted_rate)

  ## calculate for whole region
  res <- list() ## create empty list to save result

  for (comn_i in 1:(dim(x_combn)[1])) {
    ## check if TRUE
    id <- apply(x[,by], 1, function(x) !(FALSE %in% (x == x_combn[comn_i,])))

    n_age_adj <- sum(x[id, "age_adjusted_rate"])
    n_pop <- sum(x[id, "POP"])
    n_rate <- sum(x[id, "rate"])
    n_dens <- sum(x[id, "popdens"])

    res[[comn_i]] <- dplyr::tibble(x_combn[comn_i,], age_adjusted_rate = n_age_adj,
                            POP = n_pop, rate = n_rate, popdens = n_dens,
                            YEAR = year_ref,
                            INDICATOR = "myocardial infarction incidence")
  }

  x <- dplyr::bind_rows(x, dplyr::bind_rows(res))

  ## return result
  return(x)
}

#' Calculate difference in percentages (%)
#'
#' @param x The reference proportion.
#' @param y The most recent value upon which the difference should be calculated.
#' @param  dec The number of decinimal numbers to be used.
#' @param as_text Should the result be transformed as text.
#' @return difference (in %).
#' @export
diff_perc <- function(x, y, dec = 1, as_text = TRUE) {
  if (FALSE %in% unlist(lapply(list(x, y), is.numeric))) {
    stop("Input for x and y should be numeric")
  }
  ## calculate
  res <- round(((y-x)/x)*100, dec)

  ## textual?
  if (as_text) {
    res <- paste0(res, " %")
  }
  ## return result
  return(res)
}

#' Save the DW calculation as an xlsx-workbook
#'
#' @param DW result of the DW calculation (tibble)
#' @param folder folder, where to save the file (not needed if folder structure is integrated in name)
#' @param name filename to save the file (no .xlsx extension needed)
#' @param use_date should the date be included in the file (Default: TRUE)
#' @param template template that should be used when saving the file
#' @return xlsx-file with on each tab the DW for each disease
#' @export
save_dw_xlsx <- function(DW, folder = "", name = "", use_date = TRUE, template){

  ## copy worksheet style
  xlsx_template <- template

  ## change the order of the output
  id_c <- c("Age","Sex","DIA","AMI","ANG","CBDA","CBDC","CKD","COPD","LBP","NKP","OST","RHE","ALZ","EPI",
            "ALD","AMP","OPI","CAN","COC","SCH","MDD","DYS","ANX","BIP","CIR","MIG","TTH","AST","HEA","MAC","CAT",
            "GLA","NVL","REF") %in% colnames(DW)

  DW <- DW[c("Age","Sex","DIA","AMI","ANG","CBDA","CBDC","CKD","COPD","LBP","NKP","OST","RHE","ALZ","EPI",
             "ALD","AMP","OPI","CAN","COC","SCH","MDD","DYS","ANX","BIP","CIR","MIG","TTH","AST","HEA","MAC","CAT",
             "GLA","NVL","REF")[id_c]]

  ## write sheet
  writeData(xlsx_template, "DW", DW, withFilter = TRUE)

  ## filename
  if (use_date) {
    fname <- paste0(folder, name, "-",format(Sys.Date(), "%Y%m%d"), ".xlsx")
  } else {
    fname <- paste0(folder, name, ".xlsx")
  }
  ## write as xlsx-file
  saveWorkbook(xlsx_template,
               file = fname,
               overwrite = TRUE)

  message(paste0("File succesfully saved as " ,fname))

}
