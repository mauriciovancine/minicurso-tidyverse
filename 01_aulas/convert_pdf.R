# -------------------------------------------------------------------------
# script convert to xaringan presentation to pdf

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# directory
setwd("/home/mude/data/github/minicurso-r-sebio-2019/01_aulas")
dir()

# convert rmarkdown
purrr::map(dir(pattern = ".Rmd")[4:6], pagedown::chrome_print)

# convert html
# purrr::map(dir(pattern = ".html"), pagedown::chrome_print)

# end ---------------------------------------------------------------------