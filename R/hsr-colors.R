##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  hsr-colors.R
##  Purpose: load colors used in HSR reports
##  Author: Robby De Pauw
##  Date: 04/04/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' hsr_colors
#'
#' @return HSR COLORS
#' @export
hsr_colors <- function() {

  hsr_colors <- list()
  ## define colors
  hsr_colors[["col_be"]] <- "black"
  hsr_colors[["col_br"]] <- "#0059ff"  # complementary to HTML 'orange'; IRIS blue: '#00247d'
  hsr_colors[["col_fl"]] <- "#ffa500"  # HTML 'orange' color
  hsr_colors[["col_wa"]] <- "#ff0f21"
  hsr_colors[["col_mw"]] <- "#000000"
  hsr_colors[["col_m"]]  <- "#2980b9"
  hsr_colors[["col_w"]]  <- "#d35400"
  hsr_colors[["col_cat2"]] <- RColorBrewer::brewer.pal(4, "Blues")[c(4, 2)]
  hsr_colors[["col_cat5"]] <- tail(RColorBrewer::brewer.pal(8, "Blues"), 5)
  hsr_colors[["col_cat7"]] <- tail(RColorBrewer::brewer.pal(9, "Blues"), 7)
  hsr_colors[["col_oecd"]] <- hsr_colors$col_cat2[2]
  hsr_colors[["col_oecd_be"]]  <- RColorBrewer::brewer.pal(4, "Reds")[4]
  hsr_colors[["col_oecd_be2"]] <- RColorBrewer::brewer.pal(4, "Reds")[2]
  hsr_colors[["col_oecd_eu"]]  <- RColorBrewer::brewer.pal(4, "Greys")[4]
  hsr_colors[["col_oecd_eu2"]] <- RColorBrewer::brewer.pal(4, "Greys")[2]

  hsr_colors[["col_hc"]] <-
    c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9",
      "#f15c80", "#e4d354", "#2b908f", "#f45b5b", "#91e8e1")

  return(hsr_colors)
}



