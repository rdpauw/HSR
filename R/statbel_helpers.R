#' Perform the datamanagement of the STATBEL dataset for the BeBod-project
#'
#' @param df_statbel raw STATBEL dataset that can be read in by the open.xlsx package.
#' @param year reference year for which the population has to be calculated.
#' @return STATBEL dataset
#' @export
statbel_management <- function(df_statbel, year){
  year <- as.character(year)

  ## id the year
  id.1 <- which(grepl(x = names(df_statbel), pattern = year))
  id.2 <- which(names(df_statbel) %in% c("Age", "Region", "Gender"))
  id <- c(id.2, id.1)

  ## only keep necessary info
  df_statbel <- df_statbel[,id]

  ## remove missing data
  df_statbel <- na.omit(df_statbel)

  ## change variables
  df_statbel$Region <-
    sapply(df_statbel$Region,
           function(x)
             switch(x,
                    "Flanders region" = "Flanders",
                    "Brussels-Capital region" = "Brussels",
                    "Walloon region" = "Wallonia"))
  df_statbel$Region <- unlist(df_statbel$Region)

  df_statbel$Gender <-
    factor(df_statbel$Gender, c("Men", "Women"), c("M", "F"))

  df_statbel$Age <- sub(pattern = " years", x = df_statbel$Age, replacement = "")
  df_statbel$Age <- sub(pattern = " year", x = df_statbel$Age, replacement = "")
  df_statbel$Age <- sub(pattern = "Less than 1", x = df_statbel$Age, replacement = "0")
  df_statbel$Age <- sub(pattern = "100 and more", x = df_statbel$Age, replacement = "100")

  ## categorize age
  df_statbel$Age <- getage_num(as.numeric(df_statbel$Age))

  ## renaming
  names(df_statbel) <- c("Age", "Region", "Sex", "POP")

  ## grouping and summarizing
  df_statbel <- summarise(group_by(.data = df_statbel, Age, Region, Sex), POP = sum(POP))

  ## return result
  return(df_statbel)

}
