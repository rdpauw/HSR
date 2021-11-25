#' Calculate YLD
#'
#' @param x Short code of the disease for which YLD should be calculated
#' @param f_prev File with prevalence estimates
#' @param f_dw File with calculated DW (disease model)
#' @param POP Population datafile from STATBEL (cleaned)
#' @param folder Folder to save output
#' @return Saved csv-file
#' @export
getYLD <-
  function(x, f_prev, f_dw, POP, folder) {
    PREV <- openxlsx::read.xlsx(f_prev, x)
    DW   <- openxlsx::read.xlsx(f_dw, "DW")

    ## change colname of dataset
    colnames(PREV)[which(colnames(PREV) == x)] <- "P"

    ## create empty dataframe
    YLD <- data.frame()

    # apply P to each age group
    for (i in seq(nrow(PREV))) {
      a <- getage(PREV[i, "Age"])
      p <- PREV[rep(i, length(a)), c("Region", "Sex", "Age", "P")]
      p$Age <- a
      YLD <- rbind(YLD, p)
    }

    ## join YLD with POP
    YLD <- left_join(YLD, POP, by = c("Region", "Sex", "Age"))

    ## calculate P*POP
    YLD$N   <- unlist(YLD$P) * unlist(YLD$POP)
    DW_tmp <- DW[c("Age", "Sex", x)]
    names(DW_tmp)[3] <- "DW"

    YLD <- left_join(YLD, DW_tmp, by = c("Sex", "Age"))
    YLD$YLD <- YLD$N * as.numeric(YLD$DW)

    write.csv2(YLD, file = paste0(folder, x, "-", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = F)

    print(
      knitr::kable(
        format.args = list(big.mark = " "),
        digits = 0,
        summary1(YLD)))
  }

#' Generate summary
#'
#' @param x Summary table from YLD (used by getYLD function)
#' @return printed table from YLD calculations
#' @export
summary1 <-
  function(x) {
    DEN <- c(with(x, tapply(POP, Region, sum)), BELGIUM = sum(x$POP))

    N_REG <- c(with(x, tapply(N, Region, sum)), BELGIUM = sum(x$N))
    Y_REG <- c(with(x, tapply(YLD, Region, sum)), BELGIUM = sum(x$YLD))

    N_REG_RT <- 1e5 * N_REG / DEN
    Y_REG_RT <- 1e5 * Y_REG / DEN

    POPTOT <- aggregate(POP ~ Age, x, sum)
    POPTOT <- POPTOT[match(unique(POP$Age), POPTOT$Age), ]
    x$N_STD <- (x$N / x$POP) * POPTOT$POP
    x$YLD_STD <- (x$YLD / x$POP) * POPTOT$POP

    Y_REG_STD <-
      c(with(x, tapply(YLD_STD, Region, sum)), BELGIUM = sum(x$YLD_STD))
    DEN_STD <- c(rep(2*sum(POPTOT$POP), 3), 3*2*sum(POPTOT$POP))

    Y_REG_RT_STD <- 1e5 * Y_REG_STD / DEN_STD

    data.frame(
      Cases = N_REG,
      YLD = Y_REG,
      Cases.RT = N_REG_RT,
      YLD.RT = Y_REG_RT,
      YLD.ASRT = Y_REG_RT_STD)
  }
