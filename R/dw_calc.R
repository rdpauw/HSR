#' Perform the standard DW calculations (non-stratified)
#'
#' @param xlsx Excel-file with sheets named as in input dis.
#' @param dis names of the variables that should be extracted to calculate the disease DW (!should be character string).
#' @return List with resulting DW age- and gender stratified for each of the given diseases.
#' @export
dw_calc <- function(xlsx, dis){
  ## create an empty list
  dw_dat <- list()
  ## loop over all available disease states
  message("Calculating DW...\n")
  pb = txtProgressBar(title = "Calculating DW",
                      min = 0, max = length(dis), initial = 0) ## add progress bar
  setTxtProgressBar(pb,0)
  i <- 1
  for (dis in diseases) {
    ## read data
    tmp <- read.xlsx(xlsx, sheet = dis, check.names = TRUE)
    ## check if sum of proportions equals 1
    if (sum(tmp$Proportion) != 1) {
      warning(paste0("Sum of proportion in disease ", dis, " does not equal 1"))
    }
    ## ONLY CALCULATE IF DATA ARE AVAILABLE
    if (!is.null(tmp)) {
      ## calculate DW over all states
      res <- tibble(start = rep(1, dim(tmp[,-1])[1]))
      ## multiply each column
      for (c in 2:dim(tmp)[2]) {
        res$start <- res$start*tmp[c]
      }
      ## sum all disabilities
      dw_dat[[paste0(dis)]] <- sum(res$start)
    }
    setTxtProgressBar(pb,i)
    i <- i + 1
  }
  ## cbind the list
  dw_dat <- as_tibble(rbind(dw_dat))
  ## clean the result
  age <-
    c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
      "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
  sex <-
    c("M", "F")

  message("Calculation finished!")

  res_DW <- as_tibble(expand.grid(age,sex))
  res_DW <- merge(res_DW, dw_dat)
  names(res_DW)[1:2] <- c("Age", "Sex")

  return(res_DW)
}
