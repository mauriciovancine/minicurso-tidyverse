# -------------------------------------------------------------------------
# aula 4 - introducao ao tidyverse

# mauricio vancine
# 23-10-2019
# -------------------------------------------------------------------------

# topicos -----------------------------------------------------------------  
# 3.1 tidyverse
# 3.2 magrittr (pipe - %>%)
# 3.3 readr
# 3.4 readxl e writexl
# 3.5 tibble
# 3.6 tidyr
# 3.7 dplyr
# 3.8 forcats
# 3.9 lubridate
# 3.10 stringr
# 3.11 purrr

# 3.1 tidyverse -----------------------------------------------------------
# instalar o pacote
# install.packages("tidyverse")

# carregar o pacote
library(tidyverse)

# 3.2 magrittr (pipe - %>%) -----------------------------------------------
# sem pipe
sqrt(sum(1:100))

# com pipe
1:100 %>% 
  sum() %>% 
  sqrt()

# fixar amostragem
set.seed(42)

# sem pipe
ve <- sum(sqrt(sort(log10(rpois(100, 10)))))
ve

# fixar amostragem
set.seed(42)

# com pipe
ve <- rpois(100, 10) %>% 
  log10() %>%
  sort() %>% 
  sqrt() %>% 
  sum()
ve 

# exercicio 09 ------------------------------------------------------------


# 3.3 readr ---------------------------------------------------------------
# diretorio
setwd("/home/mude/data/github/minicurso-tidyverse/03_dados")

# formato .csv
# import sites
si <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
si

# formato .txt
# import sites
si <- readr::read_tsv("ATLANTIC_AMPHIBIANS_sites.txt")
si

# 3.4 readxl e writexl ----------------------------------------------------
# packages
# nstall.packages("readxl")
library("readxl")

# install.packages("writexl")
library("writexl")

# import sites
si <- readxl::read_xlsx("ATLANTIC_AMPHIBIANS_sites.xlsx")
si

# 3.3 readr ---------------------------------------------------------------
# diretorio
setwd("/home/mude/data/github/minicurso-tidyverse/03_dados")

# import sites
si <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
si

# import sites
sp <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
sp

# 3.5 tibble --------------------------------------------------------------
# view the data
tibble::glimpse(si)

# 1. nunca converte um tipo character como factor
df <- data.frame(ch = c("a", "b"), nu = 1:2)
str(df)

tb <- tibble(ch = c("a", "b"), nu = 1:2)
glimpse(tb)

# 2. A indexação com colchetes retorna um tibble
df_ch <- df[, 1]
class(df_ch)

tb_ch <- tb[, 1]
class(tb_ch)

tb_ch <- tb$ch
class(tb_ch)

# 3. Não faz correspondência parcial, retorna NULL se a coluna não existe com o nome especificado
df$c
tb$c

# 3.6 tidyr ---------------------------------------------------------------
# funcoes
# 1 unite(): junta dados de multiplas colunas em uma
# 2 separate(): separa caracteres em mulplica colunas
# 3 separate_rows(): separa caracteres em múlplica colunas e linhas
# 4 drop_na(): retira linhas com NA
# 5 replace_na(): substitui NA
# 6 spread() => pivot_wider(): long para wide
# 7 gather() => pivot_longer(): wide para long

# 1 unite
# unir as colunas latirude e longitude separadas por uma vírgula
# sem pipes
si_unite <- tidyr::unite(si, "lat_lon", latitude:longitude, sep = ",")
si_unite$lat_lon

# com pipes
si_unite <- si %>% 
  tidyr::unite("lat_lon", latitude:longitude, sep = ",")
si_unite$lat_lon
  
# 2 separate
# separar os dados de "period" em quatro colunas dos seus valores
si_separate <- si %>% 
  tidyr::separate("period", c("mo", "da", "tw", "ni"), remove = FALSE)
si_separate[, c(1, 9:13)]

# 3 separate_rows()
si_separate_row <- si %>% 
  tidyr::separate_rows("period")
si_separate_row[, c(1, 9:13)]

# 4 drop_na()
# remove as linhas com NA de todas as colunas
si_drop_na <- si %>% 
  tidyr::drop_na()
si_drop_na

# remove as linhas com NA da coluna "active_methods"
si_drop_na <- si %>% 
  tidyr::drop_na(active_methods)
si_drop_na

# 5 replace_na()
# substituir os NAs da coluna "active_methods" por 0 
si_replace_na <- si %>% 
  tidyr::replace_na(list(active_methods = 0))
si_replace_na

# 6 spread()
si[, c("id", "state_abbreviation", "species_number")]
si_spread <- si[, c("id", "state_abbreviation", "species_number")] %>% 
  tidyr::spread(key = state_abbreviation, value = species_number, fill = 0)
si_spread

sp[1:1000, c("id", "species", "individuals")]
sp_spread <- sp[1:1000, c("id", "species", "individuals")] %>% 
  tidyr::spread(key = species, value = individuals, fill = 0)
sp_spread

# 6 pivot_wider
si[, c("id", "state_abbreviation", "species_number")]
si_wide <- si[, c("id", "state_abbreviation", "species_number")] %>% 
  tidyr::pivot_wider(names_from = state_abbreviation, values_from = species_number, values_fill = list(species_number = 0))
si_wide

sp[1:1000, c("id", "species", "individuals")]
sp_wide <- sp[1:1000, c("id", "species", "individuals")] %>% 
  tidyr::pivot_wider(names_from = species, values_from = individuals, values_fill = list(individuals = 0))
sp_wide

# 7 gather()
si_gather <- si_spread %>% 
  tidyr::gather(key = record, value = species_number, -id)
si_gather

# 7 pivot_longer()
si_long <- si_wide %>% 
  tidyr::pivot_longer(cols = -id, names_to = "record", values_to = "species_number")
si_long

# exercicio 10 ------------------------------------------------------------



# 3.7 dplyr ---------------------------------------------------------------
# funcoes
# 1 select()**: seleciona colunas pelo nome gerando um tibble<br>
# 2 pull()**: seleciona uma coluna como vetor<br>
# 3 rename()**: muda o nome das colunas<br>
# 4 mutate()**: adiciona novas colunas ou adiciona resultados em colunas existentes<br>
# 5 arrange()**: reordenar as linhas com base nos valores de colunas<br>
# 6 filter()**: seleciona linhas com base em valores<br>
# 7 distinc()**: remove linhas com valores repetidos com base nos valores de colunas<br>
# 8 slice()**: seleciona linhas pelos números<br>
# 9 sample_n()**: amostragem aleatória de linhas<br>
# 10 summarise()**: agrega ou resume os dados através de funções, podendo considerar valores das colunas<br>
# 11 join()**: junta dados de duas tabelas através de uma coluna chave

# 1 select
# seleciona colunas pelo nome
si_select <- si %>% 
  dplyr::select(id, longitude, latitude)
si_select

# nao seleciona colunas pelo nome
si_select <- si %>% 
  dplyr::select(-c(id, longitude, latitude))
si_select

#  starts_with(), ends_with() e contains()
si_select <- si %>% 
  dplyr::select(contains("sp"))
si_select

# 2 pull
# coluna para vetor
si_pull <- si %>% 
  dplyr:: pull(id)
si_pull

si_pull <- si %>% 
  dplyr::pull(species_number)
si_pull

# 3 rename
si_rename <- si %>%
  dplyr::rename(sp = species_number)
si_rename

# 4 mutate
si_mutate <- si %>% 
  dplyr::mutate(record_factor = as.factor(record))
si_mutate

# 5 arrange
si_arrange <- si %>% 
  dplyr::arrange(species_number)
si_arrange

si_arrange <- si %>% 
  dplyr::arrange(desc(species_number))
si_arrange

si_arrange <- si %>% 
  dplyr::arrange(-species_number)
si_arrange

# 6 filter
si_filter <- si %>% 
  dplyr::filter(species_number > 5)
si_filter

si_filter <- si %>% 
  dplyr::filter(between(species_number, 1, 5))
si_filter

si_filter <- si %>% 
  dplyr::filter(is.na(passive_methods))
si_filter

si_filter <- si %>% 
  dplyr::filter(is.na(active_methods) & is.na(passive_methods))
si_filter

si_filter <- si %>% 
  dplyr::filter(species_number > 5 & state_abbreviation == "BR-PE") 
si_filter

si_filter <- si %>% 
  dplyr::filter(species_number > 5 | state_abbreviation == "BR-PE")
si_filter

# 7 distinct
si_distinct <- si %>% 
  dplyr::distinct(species_number)
si_distinct

si_distinct <- si %>% 
  dplyr::distinct(species_number, .keep_all = TRUE)
si_distinct

# 8 slice 
si_slice <- si %>% 
  dplyr::slice(1:10)
si_slice

# 9 n_sample 
si_sample_n <- si %>% 
  dplyr::sample_n(100)
si_sample_n

# 10 summarise
si_summarise <- si %>% 
  dplyr::summarise(mean_sp = mean(species_number), sd_sp = sd(species_number))
si_summarise

si_summarise_group <- si %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_sp = mean(species_number), sd_sp = sd(species_number))
si_summarise_group

#  permite manipular os dados de forma mais simples
da <- si %>% 
  dplyr::select(id, state_abbreviation, species_number)
da

da <- si %>% 
  dplyr::select(id, state_abbreviation, species_number) %>% 
  dplyr::filter(species_number > 5)
da

da <- si %>% 
  dplyr::select(id, state_abbreviation, species_number) %>% 
  dplyr::filter(species_number > 5) %>% 
  dplyr::group_by(state_abbreviation) %>% 
  dplyr::summarise(nsp_state_abb = n())
da

da <- si %>% 
  dplyr:: select(id, state_abbreviation, species_number) %>% 
  dplyr::filter(species_number > 5) %>% 
  dplyr::group_by(state_abbreviation) %>% 
  dplyr::summarise(nsp_state_abb = n()) %>% 
  dplyr::arrange(nsp_state_abb)
da

# exercicio 15 ------------------------------------------------------------


# exercicio 16 ------------------------------------------------------------


# exercicio 17 ------------------------------------------------------------


# exercicio 18 ------------------------------------------------------------


end ---------------------------------------------------------------------