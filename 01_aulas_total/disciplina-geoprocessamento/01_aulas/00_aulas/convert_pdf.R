# -------------------------------------------------------------------------
# script convert rmarkdown into pdf
# mauricio vancine
# 16-11-2019
# -------------------------------------------------------------------------

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# directory
setwd("/home/mude/data/github/disciplina-geoprocessamento/01_aulas/00_aulas")

# convert to pdf
purrr::map(dir(pattern = ".Rmd$"), pagedown::chrome_print)

pagedown::chrome_print("00_apres_xaringan_geoprocessamento.Rmd")

# end ---------------------------------------------------------------------