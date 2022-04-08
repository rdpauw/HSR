##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  01-updates.R
##  Purpose: perform updates of teh package
##  Author: Robby De Pauw
##  Date: 29-03-2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## !!!!! DO NOT SOURCE THIS DOCUMENT !!!!!

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        INFO ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script to create updates of the "HSR" package
## load required libraries

## The package contains the following data/functions:
## .. betaguess: guess beta parameters based on CI and mean/mode
## .. mean_ci: return the mean and 95% CI

## .. statbel: population statbel data



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
source("inst/dev/02-pop.R")

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
usethis::use_package() ## include a package as dependency
usethis::use_vignette() ## include a new vignette
