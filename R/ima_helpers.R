#' Perform the datamanagement of the IMA dataset for the BeBod-project
#'
#' @param df_his raw IMA dataset that can be read in by the haven-package.
#' @param dis names of the variables that should be extracted to calculate the disease prevalences (!should be character string)
#' @param residence residence status that should be extracted from the IMA dataset. Default value: "resident" for Belgian residents.
#' @return Clean IMA dataset
#' @export

ima_management <- function(df_ima, dis, residence = "resident"){
  ## Check that at least year, wfin, age, region, and sex is defined within the HIS dataset
  id <- c("REGION", "RESIDENCE", "AGE", "GENDER", "total") %in% names(df_ima)
  if (FALSE %in% id) {
    nmiss <- sum(!id)
    varmiss <- c("REGION", "RESIDENCE", "AGE", "GENDER", "total")[!id]
    stop(paste0("There", ifelse(nmiss == 1, "is", "are"), nmiss, "variable(s) missing:", varmiss))
  }

  ## filter the data and create clean dataset
  ### if NA in names, remove:
  dis <- dis[!is.na(dis)]
  df_ima <- df_ima[c(c("REGION", "RESIDENCE", "AGE", "GENDER", "total"), dis)]

  ### filter the year
  if (residence[1] != "all") {
    if (FALSE %in% (residence %in% unique(df_ima$RESIDENCE))) {
      warning("Unavailable levels will be skipped. Please take note of the following possibilities: ", paste0(unique(df_ima$RESIDENCE), ", "))
      id.y <- residence[(residence %in% unique(df_ima$RESIDENCE))]
    } else {
      id.y <- residence
    }
    if (length(id.y) > 0) {
      df_ima <- df_ima[df_ima$RESIDENCE %in% id.y,]
    } else {
      warning("No correct residence specified. Returning all residences: ", paste0(unique(df_ima$RESIDENCE), ", "))
    }

  }
  ### redefine contextual indicators
  df_ima$GENDER <-
    factor(df_ima$GENDER, c(1, 2), c("M", "F"))

  df_ima$AGE_CAT <-
    getage_num(df_ima$AGE)

  df_ima$AGE_CAT8 <-
    cut(df_ima$AGE, c(0, 15, 25, 35, 45, 55, 65, 75, Inf), right = FALSE,
        labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))

  ## return result
  return(df_ima[c("REGION", "GENDER", "AGE_CAT", "AGE_CAT8", dis, "total")])

}

#' Perform prevalence calculations for the HIS dataset
#'
#' @param df_ima cleaned BEHIS dataset that resulted from the cleaning in the his_management-function.
#' @param IMA dataframe containing the following columns: ID - ID number, DIS - abbr. disease, HIS - name of DIS in the HIS dataset, and SUBJ - should subjective health be taken into account.
#' @return list of prevalence estimates for each disease that is included in the HIS dataset (NA will be discarted).
#' @export
ima_prev_calc <- function(df_ima, IMA){
  result <- list()

  ## loop over all IMA prevalences
  prev_ima <- unlist(na.omit(IMA)["DIS"])

  ## progress bar
  message("Calculating PREV...\n")
  pb = txtProgressBar(title = "Calculating PREV",
                      min = 0, max = length(prev_ima), initial = 0) ## add progress bar
  setTxtProgressBar(pb,0)
  i <- 1
  ## loop over all diseases
  for (dis in prev_ima) {

    ## check name in IMA
    IMA_name <- IMA[(IMA$DIS == dis), "PSEUDOPATH"]

    ## filter dataset
    df_tmp <- df_ima[,
                     c("REGION", "GENDER", "AGE_CAT", "AGE_CAT8", IMA_name, "total")]


    ## group the dataset by region, gender and age
    suppressMessages(tmp <- summarize(group_by(df_tmp, REGION, GENDER, AGE_CAT), count = sum(!! rlang::sym(paste(IMA_name))), total = sum(total)))

    ## Calculate prevalence
    suppressWarnings(res <- prop.test(x = tmp$count, tmp$total))
    tmp[dis] <- res$estimate

    ## select only nec vars
    p <- tmp[c("REGION", "GENDER", "AGE_CAT", dis)]

    ## rename
    names(p)[1:3] <- c("Region", "Sex", "Age")

    ## create list with information
    result[[dis]] <- p

    setTxtProgressBar(pb,i)
    i <- i + 1
  }

  message("\n")

  return(result)
}
