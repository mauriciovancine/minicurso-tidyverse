#' ---
#' title: instalar pacotes
#' author: mauricio vancine
#' date: 2020-04-25
#' ---

# packages ----------------------------------------------------------------

# github ------------------------------------------------------------------
if(!require(devtools)) install.packages("devtools")

# tidyverse ---------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(broom)) install.packages("broom")
if(!require(lubridate)) install.packages("lubridate")  
if(!require(readxl)) install.packages("readxl")
if(!require(writexl)) install.packages("writexl")

# tidymodels  -------------------------------------------------------------
if(!require(tidymodels)) install.packages("tidymodels")

# plot --------------------------------------------------------------------
if(!require(GGally)) install.packages("GGally")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(psych)) install.packages("psych")

# data --------------------------------------------------------------------
if(!require(datasauRus)) install.packages("datasauRus")

# countdown ---------------------------------------------------------------
if(!require(countdown)) devtools::install_github("gadenbuie/countdown")

# markdown ----------------------------------------------------------------
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(knitr)) install.packages("knitr")
if(!require(tinytex)) install.packages("tinytex"); tinytex::install_tinytex()
if(!require(xaringan)) install.packages("xaringan")
if(!require(icon)) devtools::install_github("ropenscilabs/icon")
# end ---------------------------------------------------------------------