#' check and struturize dataportal file
#' @param x A data.frame or tibble
#' @return data.frame
#' @export

check_dtaportal <- function(x){
  ## check if x is a tibble or a data.frame
  if (!(is.data.frame(x)|is_tibble(x))) stop("x should be either a data.frame or tibble")

  ## check if necessary columns are present by matching the column names
  colnames(x) <- tolower(colnames(x))
  obligated <- c("year", "indicator", "cause",
                 "age", "region", "sex",
                 "metric", "measure", "value")
  additional <- c("exposure_cat", "exposure", "low", "high")

  id_col <- obligated %in% colnames(x)
  id_additional <- additional %in% colnames(x)

  if (FALSE %in% id_col)
    stop(paste0("the following obligated columns are missing: ", paste(obligated[!id_col], collapse = " ")))

  if (TRUE %in% id_col)
    warning(paste0("the following additional columns are present: ", paste(additional[id_additional], collapse = " ")))

  ## check if exposure_cat is present when exposure is present
  if (TRUE %in% c(additional[1:2] %in% colnames(x))) {
    if(FALSE %in% ((!is.na(x$exposure)&!is.na(x$exposure_cat))|
                   (is.na(x$exposure)&is.na(x$exposure_cat))))
      stop("Both exposure_cat and exposure should either be missing or non-misisng")
  }

  ## no missing data present is obligated columns
  if (sum(is.na(x[obligated]))>0) {
    stop("There is missing data present in the obligated columns")
  }

  ## upper case age
  x$age <- toupper(x$age)
  x$sex <- toupper(x$sex)

  ## check if there are illigal values
  id_sex <- x$sex %in% c("M", "F", "MF")
  id_metric <- x$metric %in% c("NR", "RT", "YLD", "YLL", "DALY")

  if (FALSE %in% id_sex) stop(paste0("SEX contains illegal values: ", unique(x$sex[!id_sex])))
  if (FALSE %in% id_metric) stop(paste0("METRIC contains illegal values: ", unique(x$metric[!id_metric])))

  ## return x
  return(x)
}
