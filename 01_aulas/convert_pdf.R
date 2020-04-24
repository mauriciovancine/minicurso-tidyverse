# -------------------------------------------------------------------------
# script convert to xaringan presentation to pdf

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# directory
setwd("/home/mude/data/github/minicurso-tidyverse/01_aulas")
dir()

# convert rmarkdown
purrr::map(dir(pattern = ".Rmd")[c(1, 2)], pagedown::chrome_print)

# convert html
# purrr::map(dir(pattern = ".html"), pagedown::chrome_print)

# end ---------------------------------------------------------------------