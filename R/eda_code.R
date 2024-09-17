################################################################################
################################################################################
#   The R code provided below for demonstrating the exploration of data can be 
#   used as is, or modified as needed. 
################################################################################
################################################################################

# Load this library, automatically installed if not present in local machine
if(!(require("pacman"))){install.packages("packman",
                                          dependencies=TRUE)}

pacman::p_load("dplyr",
              "magrittr",
              "ggplot2",
              "tidyverse",
              "readr")

data.sim <- readr::read_csv("data/mockdata_cases1.csv")
