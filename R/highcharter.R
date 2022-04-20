##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  highcharter.R
##  Purpose: extra highcharter functions
##  Author: Robby De Pauw
##  Date: 04/04/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Highcharter fx: drop padding
#'
#' @param x A highcharter chart
#' @return A highchater chart.
#' @export
drop_padding <-
  function(x) {
    x$sizingPolicy <- htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE)
    return(x)
  }

#' Highcharter fx: drop yAxis
#'
#' @param x A highcharter chart
#' @return A highchater chart.
#' @export
drop_yAxis <-
  function(x) {
    x$x$hc_opts$yAxis$title <- list(text = NULL)
    return(x)
  }

#' Highcharter fx: drop xAxis
#'
#' @param x A highcharter chart
#' @return A highchater chart.
#' @export
drop_xAxis <-
  function(x) {
    x$x$hc_opts$xAxis$title <- list(text = NULL)
    return(x)
  }

#' Highcharter fx: export
#'
#' @param hc A highcharter chart
#' @param filename Filename used for saving
#' @param map Are you exporting a map? (Default: \code{FALSE})
#' @return Saved highcharter chart in .js
#' @export
export_hc2 <-
  function(hc, filename, map = FALSE) {
    ## derive filename
    f <- gsub(".*/", "", gsub(".js", "", filename))

    ## original code
    . <- NULL
    stopifnot(!is.null(filename))
    if (!stringr::str_detect(filename, ".js$"))
      filename <- stringr::str_c(filename, ".js")
    jslns <-
      hc$x$hc_opts %>%
      jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE,
             force = TRUE, null = "null") %>%
      stringr::str_split("\n") %>%
      utils::head(1) %>%
      unlist() %>%
      stringr::str_replace("\"", "") %>%
      stringr::str_replace("\":", ":")

    ## add quotes when needed
    id <- grepl(".*-.*: ", jslns)
    jslns[id] <- gsub("(^ *)(.*): ", "\\1\"\\2\": ", jslns[id])

    ## continue original code
    fflag <- stringr::str_detect(jslns, "function()")
    if (any(fflag)) {
      jslns <-
        ifelse(fflag, stringr::str_replace(jslns, "\"function", "function"), jslns)
      jslns <-
        ifelse(fflag, stringr::str_replace(jslns, "\",$", ","), jslns)
      jslns <-
        ifelse(fflag, stringr::str_replace(jslns, "\"$", ""), jslns)
      jslns <-
        ifelse(fflag,
               stringr::str_replace_all(
                 jslns,
                 "\\\\n",
                 stringr::str_c("\\\\n", stringr::str_extract(jslns, "^\\s+"))),
               jslns)
    }
    jslns <-
      jslns %>%
      unlist() %>%
      utils::tail(-1) %>%
      stringr::str_c("    ", ., collapse = "\n") %>%
      stringr::str_replace_all("\n\\s{4,}\\]\\,\n\\s{4,}\\[\n\\s{4,}", "],[")

    if (map) {
      jslns <-
        jslns %>%
        sprintf(
          "$(function () {\n  $('#%s').highcharts('Map', {\n%s\n  );\n});",
          f, .)

    } else {
      jslns <-
        jslns %>%
        sprintf(
          "$(function () {\n  $('#%s').highcharts({\n%s\n  );\n});", f, .)
    }

    ## fix bug with colors and quotes
    jslns <- gsub('#', '"#', jslns)
    jslns <- gsub('""', '"', jslns)
    jslns <- gsub('\'"#', '\'#', jslns)

    ## fix bug with NA's
    jslns <- gsub('"NA"', 'null', jslns)

    ## write file
    writeLines(jslns, filename)
  }
