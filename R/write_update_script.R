#' write_update_script
#'
#' @return R file to update R package
#' @export


write_update_script <- function(){
  ## check if dir exist
  if(!dir.exists("inst/dev")) {
    message("creating dir 'inst/dev'")
    dir.create("inst/dev", recursive = TRUE)
  }

  write_update_script <- "
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  00-update-package.R
##  Purpose:
##  Author: Robby De Pauw
##  Date:
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## !!!!! DO NOT SOURCE THIS DOCUMENT !!!!!

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        INFO ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script to create updates of the 'HSR' package
## load required libraries

## The package contains the following data/functions:
## ..

## ..



##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        PREPARE ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## .. load packages ####
library(devtools) ## develop the package
library(usethis) ## use for easy pkg-building
library(pkgdown) ## create a page for the package
library(readr)
library(tidyverse)
library(xlsx)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        SCRIPT ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## update the color database

## NOW YOU CAN UPDATE THE PKG

## document the changes
devtools::document()

## Perform some checks
devtools::load_all()
devtools::check()

## clean and build vignettes
devtools::clean_vignettes()
devtools::build_vignettes() ## update vignettes
pkgdown::build_site() ## update website

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        Extra code ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
usethis::use_package('') ## include a package as dependency
usethis::use_vignette() ## include a new vignette"
  writeLines(write_update_script, "inst/dev/00-update-package.R")

}
