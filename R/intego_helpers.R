#' Perform the datamanagement of the INTEGO dataset for the BeBod-project
#'
#' @param df_intego raw INTEGO dataset that can be read in by the open.xlsx package..
#' @return INTEGO dataset
#' @export
intego_management <-
  function(df_intego){
    ## fill out missing values (first take last known value, then previous known value)
    df_intego <- tidyr::fill(data = df_intego, everything(), .direction = "updown")

    df_intego$sex <-
      factor(df_intego$sex, c("M", "F"), c("M", "F"))

    df_intego$age_group <-
      factor(sub(x = df_intego$age_group, pattern = "_", "-"))

    ## change names and delete Year
    df_intego <- df_intego[,-which(names(df_intego) == "Year")]
    names(df_intego)[names(df_intego) %in% c("sex", "age_group")] <- c("Sex", "Age")

    # ## add regions
    # Region <- rep(c("Brussels", "Flanders", "Wallonia"), each = nrow(df_intego))
    # df_intego <- bind_rows(bind_rows(df_intego, df_intego),df_intego)
    # df_intego$Region <- Region

    ## divide by 100
    df_intego <- dplyr::mutate(.data = df_intego, across(.cols = -c("Sex", "Age"), .fns = ~.x/100))

    ## return result
    return(df_intego)

  }
