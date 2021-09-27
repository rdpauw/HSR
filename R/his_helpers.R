#' Perform the datamanagement of the HIS dataset for the BeBod-project
#'
#' @param df_his raw BEHIS dataset that can be read in by the haven-package.
#' @param dis names of the variables that should be extracted to calculate the disease prevalences (!should be character string)
#' @param year years that should be extracted from the HIS dataset. Default value: "all" for all years.
#' @return Clean HIS dataset
#' @export
his_management <-
  function(df_his, dis, year = "all"){
    ## Check that at least year, wfin, age, region, and sex is defined within the HIS dataset
    id <- c("wfin", "year", "hc_01", "hc04", "regio", "hh_cluster", "SH01_2") %in% names(df_his)
    if (FALSE %in% id) {
      nmiss <- sum(!id)
      varmiss <- c("wfin", "year", "hc_01", "hc04", "regio", "hh_cluster", "SH01_2")[!id]
      stop(paste0("There", ifelse(nmiss == 1, "is", "are"), nmiss, "variable(s) missing:", varmiss))
    }

    ## filter the data and create clean dataset
    ### if NA in names, remove:
    dis <- dis[!is.na(dis)]
    df_his <- df_his[c(c("wfin", "year", "hc_01", "hc04", "regio", "hh_cluster", "SH01_2"), dis)]
    ### filter the year
    if (year[1] != "all") {
      if (FALSE %in% (as.numeric(year) %in% unique(df_his$year))) {
        warning("Unavailable years will be skipped. Please take note of the following possibilities: ", paste0(unique(df_his$year), ", "))
        id.y <- as.numeric(year)[(as.numeric(year) %in% unique(df_his$year))]
      } else {
        id.y <- as.numeric(year)
      }
      if (length(id.y) > 0) {
        df_his <- df_his[df_his$year %in% as.numeric(id.y),]
      } else {
        warning("No correct year specified. Returning all years: ", paste0(unique(df_his$year), ", "))
      }

    }
    ### redefine contextual indicators
    df_his$regio <-
      sapply(df_his$regio,
             function(x)
               switch(x,
                      "1" = "Flanders",
                      "2" = "Brussels",
                      "3" = "Wallonia"))
    df_his$sex <-
      factor(df_his$hc04, c(1, 2), c("M", "F"))

    df_his$age_cat <-
      getage_num(df_his$hc_01)

    df_his$age_cat8 <-
      cut(df_his$hc_01, c(0, 15, 25, 35, 45, 55, 65, 75, Inf), right = FALSE,
          labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))

    ### recode each disease
    pb = txtProgressBar(title = "Reformatting all diseases",
                        min = 0, max = length(dis), initial = 0) ## add progress bar
    setTxtProgressBar(pb,0)
    for (i in seq_along(dis)) {
      ## define disease indicator
      df_his[dis[i]][df_his[dis[i]] < 0] <- NA
      df_his[dis[i]][df_his[dis[i]] == 1] <- 1
      df_his[dis[i]][df_his[dis[i]] == 2] <- 0

      ## update progress bar
      setTxtProgressBar(pb,i)
    }


    ## return result
    return(df_his[c("regio", "age_cat", "age_cat8", "sex", "year", "wfin", "hh_cluster", "SH01_2", dis)])

  }

#' Perform prevalence calculations for the HIS dataset
#'
#' @param df_his cleaned BEHIS dataset that resulted from the cleaning in the his_management-function.
#' @param HIS dataframe containing the following columns: ID - ID number, DIS - abbr. disease, HIS - name of DIS in the HIS dataset, and SUBJ - should subjective health be taken into account.
#' @param ref_year Reference year for the calculation based on the HIS.
#' @return list of prevalence estimates for each disease that is included in the HIS dataset (NA will be discarted).
#' @export
his_prev_calc <- function(df_his, HIS, ref_year){
  result <- list()

  ref_year <- as.character(ref_year)

  ## loop over all HIS prevalences
  prev_his <- unlist(na.omit(HIS)["DIS"])

  ## progress bar
  message("Calculating PREV...\n")
  pb = txtProgressBar(title = "Calculating PREV",
                      min = 0, max = length(prev_his), initial = 0) ## add progress bar
  setTxtProgressBar(pb,0)
  i <- 1
  ## loop over all diseases
  for (dis in prev_his) {

    ## check name in HIS
    HIS_name <- HIS[(HIS$DIS == dis), "HIS"]

    ## filter dataset
    df_tmp <- df_his[
      (df_his$year == ref_year),
      c("wfin", "sex", "regio", "hh_cluster", "age_cat", "age_cat8", "SH01_2")
    ]

    df_tmp$dis <- unlist(df_his[(df_his$year == ref_year), HIS_name])

    ## define disease indicator
    df_tmp$dis[df_tmp$dis < 0] <- NA
    df_tmp$dis[df_tmp$dis == 1] <- 1
    df_tmp$dis[df_tmp$dis == 2] <- 0

    ## Do we also want to include bad subjective health into the calculation?
    if (HIS[(HIS$DIS == dis), "SUBJ"] == "Y") {
      df_tmp$dis[!is.na(df_tmp$dis) & df_tmp$SH01_2 == 2] <- 0 # bad subjective health

    }

    ## define survey object
    hisdesign <-
      survey::svydesign(id = ~hh_cluster,
                        weights = ~wfin,
                        data = df_tmp)

    ## prevalence Belgium
    survey::svymean(~dis, hisdesign, na.rm = TRUE)
    survey::svyby(~dis, ~sex+age_cat8+regio, hisdesign, survey::svymean, na.rm = TRUE)

    #+ fig.width=12
    ## survey weighted prevalence by age, sex, region, year
    p <- survey::svyby(~dis, ~sex+regio+age_cat8, hisdesign, survey::svymean, na.rm = TRUE)
    names(p)[names(p) == "dis"] <- dis

    ## rename
    names(p)[1:3] <- c("Sex", "Region", "Age")

    ## create list with information
    result[[dis]] <- p

    setTxtProgressBar(pb,i)
    i <- i + 1
  }

  return(result)
}
