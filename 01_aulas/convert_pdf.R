# -------------------------------------------------------------------------
# script convert to xaringan presentation to pdf

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# directory
setwd("/home/mude/data/github/minicurso-tidyverse/01_aulas")
dir(pattern = ".Rmd")

# convert rmarkdown
purrr::map(dir(pattern = ".Rmd")[6], chrome_print, timeout = 2000)

# end ---------------------------------------------------------------------