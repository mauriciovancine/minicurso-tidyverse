#' ---
#' title: aula 03 - tidyverse
#' author: mauricio vancine
#' date: 2020-04-25
#' ---

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

# list all packages in the tidyverse 
tidyverse::tidyverse_packages(include_self = TRUE)

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

# import sites matrix
si <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
si

# import species matrix
sp <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
sp

# 3.5 tibble --------------------------------------------------------------
# view the data
tibble::glimpse(si)
str(si)

# 1. nunca converte um tipo character como factor
df <- data.frame(ch = c("a", "b"), nu = 1:2)
str(df)

tb <- tibble::tibble(ch = c("a", "b"), nu = 1:2)
tibble::glimpse(tb)

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
si_unite <- tidyr::unite(si, "lat_lon", latitude:longitude, sep = ",", remove = FALSE)
si_unite
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
  tidyr::replace_na(list(individuals = 0)) %>% 
  tidyr::spread(key = species, value = individuals, fill = 0)
sp_spread

# 6 pivot_wider
si_wide <- si %>% 
  tidyr::pivot_wider(id_cols = id, 
                     names_from = state_abbreviation,
                     values_from = species_number, 
                     values_fill = list(species_number = 0))
si_wide

sp_wide <- sp[1:1000, ] %>% 
  tidyr::replace_na(list(individuals = 0)) %>% 
  tidyr::pivot_wider(id_cols = id, 
                     names_from = species, 
                     values_from = individuals, 
                     values_fill = list(individuals = 0))
sp_wide

# 7 gather()
si_gather <- si_spread %>% 
  tidyr::gather(key = record, value = species_number, -id)
si_gather

# 7 pivot_longer()
si_long <- si_wide %>% 
  tidyr::pivot_longer(cols = -id, 
                      names_to = "record", 
                      values_to = "species_number")
si_long

# exercicio 10 ------------------------------------------------------------
si_unit <- si %>% 
  tidyr::unite(col = "local_total", 
               country:site,
               sep = ", ")
si_unit$local_total

# exercicio 11 ------------------------------------------------------------


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
si_mutate$record_factor

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
  dplyr::filter(is.na(active_methods))
si_filter

si_filter <- si %>% 
  dplyr::filter(!is.na(active_methods))
si_filter

si_filter <- si %>% 
  dplyr::filter(!is.na(active_methods) & !is.na(passive_methods))
si_filter

si_filter <- si %>% 
  dplyr::filter(species_number > 5 & state_abbreviation == "BR-SP") 
si_filter

si_filter <- si %>% 
  dplyr::filter(species_number > 5 | state_abbreviation == "BR-SP")
si_filter

si_filter <- si %>% 
  dplyr::filter(species_number > median(species_number, na.rm = TRUE))
si_filter

si_filter <- si %>% 
  dplyr::filter(sampled_habitat %in% c("fo,ll", "fo,la,ll"))
si_filter$sampled_habitat

si_distinct <- si %>% 
  dplyr::distinct(longitude, latitude, .keep_all = TRUE)
si_distinct

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
  dplyr::sample_n(200)
si_sample_n

set.seed(1)
si_sample_f <- si %>% 
  dplyr::sample_n(1, replace = TRUE)
si_sample_f

# 10 summarise
si_summarise <- si %>% 
  dplyr::summarise(mean_sp = mean(species_number), sd_sp = sd(species_number))
si_summarise

si_summarise_group <- si %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_sp = mean(species_number), sd_sp = sd(species_number))
si_summarise_group

# 11 *_join
# selecionar os dados
si_coord <- si %>% 
  select(id, longitude, latitude)
si_coord 

# join dos dados
sp_join <- sp %>% 
  left_join(si_coord, by = "id")
sp_join

colnames(sp_join)

plot(sp_join$longitude, sp_join$latitude, pch = 20)

# sufixos
sp_wide_rename <- sp_wide %>% 
  dplyr::rename_at(vars(contains(" ")), list(~stringr::str_replace_all(., " ", "_"))) %>% 
  dplyr::rename_all(list(~stringr::str_to_lower(.)))
sp_wide_rename

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

# exercicio 12 ------------------------------------------------------------
si <- si %>% 
  dplyr::mutate(alt_log = log10(altitude + 1),
                tem_log = log10(temperature + 1),
                pre_log = log10(precipitation + 1))
si[, 25:28]

# exercicio 13 ------------------------------------------------------------


# exercicio 14 ------------------------------------------------------------


# exercicio 15 ------------------------------------------------------------

# 3.8 stringr -------------------------------------------------------------

# comprimento
str_length("abc")

# substituicao
str_sub("abc", 3)

# inserir espaco em branco
str_pad("abc", width = 4, side = "left")
str_pad("abc", width = 4, side = "right")

# remover espaco em branco do comeco, final ou ambos
str_trim(" abc ")

# minusculas e maiusculas
str_to_upper("abc")
str_to_title("abc")
str_to_title("aBc")

# ordenarcao
le <- sample(letters, 26, rep = TRUE)
le

str_sort(le)
str_sort(le, dec = TRUE)

# extrair
str_extract("abc", "b")

# substituir
str_replace("abc", "a", "y")

# separacao
str_split("a-b-c", "-")

# exemplo
si_ex <- si %>% 
  dplyr::mutate(sampled_habitat = stringr::str_replace_all(sampled_habitat, ",", "_"))
si_ex

si_ex2 <- si %>% 
  dplyr::mutate(id_text = stringr::str_extract(id, "[a-z]+"),
                id_number = stringr::str_extract(id, "[0-9]+"),) %>% 
  dplyr::select(id, id_text, id_number, everything())
si_ex2

si_ex2 <- si %>% 
  dplyr::mutate(id_text = stringr::str_sub(id, 1, 3),
                id_number = stringr::str_sub(id, 4, 9),) %>% 
  dplyr::select(id, id_text, id_number, everything())
si_ex2

# 3.9 forcats -------------------------------------------------------------
# fixar amostragem
set.seed(1)

# cria um fator
fa <- sample(c("alto", "medio", "baixo", "muito_baixo"), 30, rep = TRUE) %>% 
  forcats::as_factor()
fa

# muda o nome dos niveis
fa_recode <- fa %>% 
  forcats::fct_recode(a = "alto", m = "medio", b = "baixo", b = "muito_baixo")
fa_recode

fa_recode <- fa %>% 
  forcats::fct_recode(a = "alto", m = "medio", b = "baixo")
fa_recode

# inverte os niveis
fa_rev <- fa_recode %>% 
  forcats::fct_rev()
fa_rev

# especifica a classificacao de um nivel
fa_relevel <- fa_recode %>% 
  forcats::fct_relevel(c("a", "m", "b"))
fa_relevel

# ordem em que aparece
fa_inorder <- fa_recode %>% 
  forcats::fct_inorder()
fa_inorder

# ordem (decrescente) de frequencia
fa_infreq <- fa_recode %>% 
  forcats::fct_infreq()
fa_infreq

# agregacao de niveis raros em um nivel
fa_lump <- fa_recode %>% 
  forcats::fct_lump()
fa_lump

# 3.10 lubridate ----------------------------------------------------------
# package
library(lubridate)

# string
data_string <- "2020-04-24"
data_string
class(data_string)

# criar um objeto com a classe data
data_date <- lubridate::date(data_string)
data_date
class(data_date)

# criar um objeto com a classe data
data_date <- lubridate::as_date(data_string)
data_date
class(data_date)

# string
data_string <- "24-04-2020"
data_string
class(data_string)

# criar um objeto com a classe data
data_date <- lubridate::dmy(data_string)
data_date
class(data_date)

# formatos
lubridate::dmy(24042020)
lubridate::dmy("24042020")
lubridate::dmy("24/04/2020")
lubridate::dmy("24.04.2020")

# especificar horarios
lubridate::dmy_h(2404202013)
lubridate::dmy_hm(240420201335)
lubridate::dmy_hms(24042020133535)

# criar
data <- lubridate::dmy_hms(24042020133535)
data

# extrair
lubridate::second(data)
lubridate::day(data)
lubridate::month(data)
lubridate::wday(data)
lubridate::wday(data, label = TRUE)

# criar
data <- lubridate::dmy(24042020)
data

# inlcuir
lubridate::hour(data) <- 13
data

# extrair a data no instante da execucao
lubridate::today() 

# extrair a data e horario no instante da execucao
lubridate::now()

# fuso horario
# agora
agora <- lubridate::ymd_hms(lubridate::now(), tz = "America/Sao_Paulo")
agora

# que horas sao em...
lubridate::with_tz(agora, tzone = "GMT")
lubridate::with_tz(agora, tzone = "Europe/Stockholm")  

# altera o fuso sem mudar a hora
lubridate::force_tz(agora, tzone = "GMT")

# operacoes com datas
# datas
inicio_r <- lubridate::dmy("30-11-2011")
hoje_r <- lubridate::today()

# intervalo
r_interval <- lubridate::interval(inicio_r, hoje_r)
r_interval
class(r_interval)

# outra forma de definir um intervalo: o operador %--%
r_interval <- lubridate::dmy("30-11-2011") %--% lubridate::today() 
namoro_interval <- lubridate::dmy("25-06-2008") %--% lubridate::today()   

# verificar sobreposicao
lubridate::int_overlaps(r_interval, namoro_interval)

# somando datas
inicio_r + lubridate::ddays(1)
inicio_r + lubridate::dyears(1)

# criando datas recorrentes
reunioes <- lubridate::today() + lubridate::weeks(0:10)
reunioes

# duracao de um intervalo 
r_interval <- inicio_r %--% lubridate::today()
r_interval

# transformacoes
r_interval / lubridate::dyears(1)
r_interval / lubridate::ddays(1)

# total do periodo estudando r
lubridate::as.period(r_interval)

# tempo de namoro
lubridate::as.period(namoro_interval)

# 3.11 purrr --------------------------------------------------------------
# list
li <- list(1:5, c(4, 5, 7), c(98, 34,-10), c(2, 2, 2, 2, 2))
li

# map
purrr::map(x, sum)

# retorna vetor
purrr::map_dbl(x, sum)

# retorna caracter
purrr::map_chr(x, paste, collapse = " ")

# duas listas
x <- list(3, 5, 0, 1)
y <- list(3, 5, 0, 1)
purrr::map2_dbl(x, y, prod)

# varias listas aninhadas
x <- list(3, 5, 0, 1)
y <- list(3, 5, 0, 1)
z <- list(3, 5, 0, 1)
purrr::pmap_dbl(list(x, y, z), prod)

# calcular a media para varaos colunas
mean_var <- si %>% 
  select(species_number, altitude) %>% 
  map_dbl(mean)
mean_var

# calcular o desvio padrao para varias colunas
mean_var <- si %>% 
  select(species_number, altitude) %>% 
  map_dbl(sd)
mean_var

# end ---------------------------------------------------------------------
