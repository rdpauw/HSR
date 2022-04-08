##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  00-pop.R
##  Purpose: create general population data.frame
##  Author: Robby De Pauw
##  Date: 31/03/2022
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        INFO ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## create a summary population data.frame based on statbel data

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        PREPARE ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## .. load data ####
dta <- readxl::read_excel("inst/extdata/20220331-pop-2006-2022.xlsx", sheet = 3)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        DATA MANAGEMENT ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## change column-names
colnames(dta) <- gsub(pattern = "Population on January 1st ", replacement = "", x = colnames(dta))

## remove age class
dta <- dta[,-3]

## restructure variables
dta$Region <- factor(dta$Region,
                     levels = c("Flanders region",
                                "Brussels-Capital region",
                                "Walloon region"),
                     labels = c("FL", "BR", "WA"))
dta$Gender <- factor(dta$Gender)
dta$Age <- as.numeric(gsub("[^0-9.-]", "", dta$Age))

## to lower
colnames(dta) <- tolower(colnames(dta))

## pivot_longer
dta <- pivot_longer(dta, cols = as.character(2006:2021), names_to = "year", values_to = "population")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        AGGREGATE ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## aggregate BE
dta_BE <- aggregate(data = dta, population ~ gender + age + year, FUN = sum)
dta_BE$region <- "BE"

## add BE to dta
statbel <- bind_rows(dta, dta_BE)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                        OUTPUT ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## save the data-set
usethis::use_data(statbel, overwrite = TRUE)
