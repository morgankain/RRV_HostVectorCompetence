## Required packages to run this code
needed_packages <- c(
    "plyr"
  , "dplyr"
  , "ggplot2"
  , "magrittr"
  , "scales"
  , "lme4"
  , "MASS"
  , "tidyr"
  , "rstan"
  , "shinystan"
  , "reshape2"
#  , "foreach"
#  , "doParallel"
#  , "data.table"
#  , "doRNG"
)

## load packages. Install all packages that return "FALSE"
lapply(needed_packages, require, character.only = TRUE)
