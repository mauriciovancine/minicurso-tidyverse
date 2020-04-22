# # -----------------------------------------------------------------------
# Objetivo: Exercícios da disciplina de Geoprocessamento
# Nome: Paulo Wanderley de Melo
# Data: 11/11/2019

# observação: comentários começando com "#---" serão lembretes meus para consultas posteriores, não fazem parte do enunciado das questões

# # -----------------------------------------------------------------------

library(tidyverse)

# 3
rm() #--- remove todos os objetos

# 4
(2 * 5) - (3 ^ 2)

# 5
log10(10) + log(100) * log2(1000)

# 6
factorial (10)
fa_10 <- factorial(10) #--- atribuir a um objeto
fa_10
sqrt (fa_10)  #--- tirar raiz quadrada de um objeto
fa_10_rq <- sqrt (fa_10)
fa_10_rq

# 7 
(400/3.5)

# 8
seq(0,10) #--- criar sequência de 0 a 10
seq_10 <- seq(0,10)
seq_10
sum(seq_10) #--- somar todos os números da sequência
seq_10_sum <- sum(seq_10)
seq_10_sum

# 9
seq(0,50, by = 5) #--- colocando espaçamento na sequência
seq_50 <- seq(0,50, by = 5)
seq_50
rep(seq_50, each = 10) #--- repetição dos númeors da sequência
seq_50_rep_times <- rep(seq_50, each = 10) 
seq_50_rep_times

# 10
mega_sena <- sample(1:60, 6) #--- gerar 6 números aleatorialmente entre 1 e 60
mega_sena

jogo_1 <- sample(mega_sena) #--- reamostrando para 2 jogos por semana por um mês
jogo_2 <- sample(mega_sena) 
jogo_3 <- sample(mega_sena) 
jogo_4 <- sample(mega_sena) 
jogo_5 <- sample(mega_sena) 
jogo_6 <- sample(mega_sena) 
jogo_7 <- sample(mega_sena) 
jogo_8 <- sample(mega_sena) 

li_mega_sena <- list(jogo_1, jogo_2, jogo_3, jogo_4, jogo_5, jogo_6, jogo_7, jogo_8) #--- criando lista onde cada elemento é um jogo aleatório
li_mega_sena

# 11
dados <- sample (1:12, 25, replace = TRUE) #--- 25 amostragens aleatorias de um dado de 12 lados

# 12
lo <- paste("local", 1:100, sep = "_") #--- criar vetor colando palavras com uma sequência numérica - lembrar das aspas
lo

# 13
lo <- paste("local", 1:100, sep = "_00")
lo

# 14 
tr <- rep(x = c("cont", "trat"), each = 50) %>%     # ---criar um lista de repetição e transforma em fator
  factor(c("cont", "trat"), 
         levels = c("cont", "trat"))    #--- criar um fator categórico de dois níveis
tr
#--- obs: x = nome do conjunto de dados; c = função "concatenar"

# 15
ve <- sample(0:10, 1000, replace = TRUE) 
ve
ma <- matrix(data = ve, nrow = 100, byrow = FALSE)
ma
#---obs: criar um vetor e depois colcoar na matriz (dispor por colunas: byrow = FALSE)

#16
#--- sem pipe
max(log(exp(sqrt(rpois(100, 5))))) 

#--- com pipe
rpois (100, 5) %>% 
  sqrt() %>% 
  exp() %>% 
  log() %>% 
  max()

#---obs: tidyverse - pipe (crtl + shift + M)

# 17
#---com pipe
rnorm(100) %>%
  exp() %>%
  log10() %>% 
  min() %>%
  sqrt()

#---sem pipe
sqrt(min(log10(exp(rnorm(100)))))

# 18
install.packages("magrittr")
library("magrittr")
round(mean(sum(1:10)/3), digits = 1) #--- sem pipes

sum (1:10) %>% 
  magrittr::divide_by(3) %>%  # --- com pipes
  mean() %>% 
  round(digits = 1)

# 19
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/03_dados/00_tabelas")
#--- definir o diretorio de trabalho - Session, set working directory, chose directory, selecionar pasta com tabelas, dar enter e pegar o setwd do console.

getwd() #--- verificar o diretorio
dir() #--- verificar os arquivos
read_csv("ATLANTIC_AMPHIBIANS_sites.csv") #--- ler uma planilha eletronica (.csv)
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

# 20
tibble::glimpse(da) #--- verificar as colunas dos dados

# 21
#--- unir multiplas colunas em uma
da_unite <- da %>% 
  unite("local_total", country:state:state_abbreviation:municipality:site, sep = ",")
da_unite$local_total

# 22
da_separate <- da %>% 
  separate("passive_methods", c("pt", "NA"), remove = FALSE)
da_separate [, c(1, 9:13)]

# 23
#--- retira os NA
da_drop_na <- da %>% 
  drop_na()
da_drop_na

# 24
#--- seleciona colunas por um nome comum
da_select <- da %>% 
  select(contains ("method"))
da_select


# 25
hist(da$species_number) #--- fazer um histograma básico

# 26
da_mutate <- da %>% #--- fazer uma operação em uma coluna e adicionar uma coluna com essa alteração
  mutate(alt_log = log10(altitude), tem_log = log10(altitude), pre_log = log10(altitude))
da_mutate
da <- da_mutate

# 27
da_arrange <- da %>% #-- ordenar os dados (decrescente) em uma coluna específica
  arrange(desc(altitude))
da_arrange$altitude #--- chamar a coluna de um objeto

# 28

da_filter <- da %>% 
  filter(altitude > 1000 | temperature < 15 | precipitation > 1000 <= 1500)
da_filter

# 29
da_sample_n <- da %>% 
  sample_n(200, species_number > 15, replace = FALSE)
da_sample_n

# 30
#-- pacote purrr
install.packages("purrr")
library("purrr")

da_range <- da %>%
  dplyr::select(species_number, altitude, temperature, precipitation) %>% #---Selecionar as colunas
  purrr::map_df(function(x) range(x, na.rm = TRUE)) #--- calcular o Range, retirando oa NA
da_range

# 31
# A função read_csv do tidyr separa as colunas por vírgula, misturando os dados, pois existem colunas que têm dados separados por vírgulas na mesma célula. 
# A solução é usar read_csv2 que separa as colunas por ponto e vírgula
read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")

# 32
library(ggplot2)
#--- correlação entre Altitude e número de espécies (regreção linear-dispersão)
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggplot(data = da) +
  aes(x = altitude, y = species_number) +
  labs(x = "Altitude", y = "Número de espécies") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("spp_number_altitude_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#--- correlação entre Temperatura e número de espécies (regreção linear-dispersão)
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggplot(data = da) +
  aes(x = temperature, y = species_number) +
  labs(x = "Temperatura", y = "Número de espécies") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("spp_number_temperatura_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#--- correlação entre Precipitation e número de espécies (regreção linear-dispersão)
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggplot(data = da) +
  aes(x = altitude, y = species_number) +
  labs(x = "Precipitação", y = "Número de espécies") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("spp_number_pracipitação_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# Existe relação positiva de ALTITUDE e PRECIPITAÇÃO e o número de espécies; e negativa entre TEMPERATURA e o número de espécies. 
# Porém, pela visualização dos gráficos, todas as três são relações baixas.

# 33
library(ggpubr)
ta <- table(da$state_abbreviation) #--- selecionar a frequência por estado
ta
ta_por <- ta %>% #--- frequência como data.frame
  as.data.frame()
colnames(ta_por) <- c("state", "freq")
ta_por

setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggplot(data = ta_por) +
  aes(x = state, y = freq) +
  geom_bar(color = ("black"), fill = ("#c51b8a"), stat = "identity") +
  labs(x = "Estados",
       y = "Frequência de amostragem") +
  coord_flip() #--- inverter a posição do gráfico
ggsave("freq_hist_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# 34
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggbarplot(data=da,
          x = "state_abbreviation",
          y = "species_number", 
          fill = "state_abbreviation", 
          color = "state_abbreviation",
          palette = c("Harmonic"),
          label = FALSE, 
          lab.size = 5,
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")
ggsave("state_spp_bar_ggpubr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# 35
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios")
ggbarplot(data=da,
          x = "coordinate_precision",
          y = "species_number", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          palette = c("PuOr"),
          label = FALSE, 
          lab.size = 5,
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none")
ggsave("gps_spp_bar_ggpubr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)


# Qgis no R ---------------------------------------------------------------

# 37
# Importar vetor
install.packages("sf")
library(sf)
library(fasterize)
library(raster)
library(tmap)
library(tidyverse)

setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R") #--- CTRL + ALT + H
rec_uso <- sf::st_read("PE_2611606_USO_topologia.shp") #--- importar vetor do diretório
rec_uso
plot(rec_uso[1]) #-- plotar para visualizar

# 38
# tabela de atributos
rec_uso_tab <-  sf::st_drop_geometry(rec_uso)
rec_uso_tab
rec_uso_tab$CLASS_USO
# selecionar feição dentro de uma coluna da tabela
rec_uso_floresta <- rec_uso %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")
rec_uso_floresta
plot(rec_uso_floresta[1])
# exportar
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R")
sf::st_write(rec_uso_floresta, "PE_2611606_USO_topologia_floresta.shp")

# 40
rec_uso_floresta_unico <- sf::st_cast(rec_uso, "POLYGON") #-- multiplas partes, para simples (ferramenta QGis)
rec_uso_floresta_unico
plot(rec_uso_floresta_unico[5]) #--- plotar
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R")
sf::st_write(rec_uso_floresta_unico, "PE_2611606_USO_topologia_floresta_unico.shp")

# 41
uso_flo_unico_id <- rec_uso_floresta_unico %>%
  dplyr::mutate(id = rec_uso_floresta_unico %>% nrow %>% seq) #--- criar uma coluna
uso_flo_unico_id

# 42
uso_flo_unico_id <- uso_flo_unico_id %>%
  dplyr::select(id) #--- selecionar uma coluna e deletar as outras
uso_flo_unico_id

# 43
uso_flo_unico_id_area <- uso_flo_unico_id %>%
  dplyr::mutate(a_frag_ha = sf::st_area(uso_flo_unico_id)/10000) #--- calcular área dos fragmentos
uso_flo_unico_id_area

# 44
uso_flo_unico_id_core <- uso_flo_unico_id %>% 
  sf::st_buffer(-60) %>% #--- buffer
  dplyr::mutate(a_core_ha = sf::st_area(.)/10000)
uso_flo_unico_id_core

tm_shape(uso_flo_unico_id_core) +
  tm_polygons()

setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R")
sf::st_write(uso_flo_unico_id_core, "PE_2611606_USO_topologia_floresta_unico_core60m.shp")

# 45
uso_flo_unico_id_edge <- uso_flo_unico_id %>% 
  sf::st_difference(sf::st_combine(uso_flo_unico_id_core)) %>% 
  dplyr::mutate(a_edge_ha = sf::st_area(.)/10000)
uso_flo_unico_id_edge

tm_shape(uso_flo_unico_id_edge) +
  tm_polygons()

setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R")
sf::st_write(uso_flo_unico_id_edge, "PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

# 46

dem <- raster::raster("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/dem_mosaico_recife_e_o_mundo2.tif")
dem
plot(dem)

uso_flo_unico_id_area_raster <- fasterize::fasterize(sf = uso_flo_unico_id_area,
                                                      raster = dem,
                                                      field = "a_frag_ha")
plot(uso_flo_unico_id_area_raster)

uso_flo_unico_id_core_raster <- fasterize::fasterize(sf = uso_flo_unico_id_core %>%
                                                        dplyr::filter(!st_is_empty(.)) %>% 
                                                        sf::st_cast("MULTIPOLYGON"),
                                                      raster = dem,
                                                      field = "a_core_ha")
uso_flo_unico_id_core_raster
plot(uso_flo_unico_id_core_raster)

uso_flo_unico_id_edge_raster <- fasterize::fasterize(sf = uso_flo_unico_id_edge %>%
                                                        dplyr::filter(!st_is_empty(.)) %>% 
                                                        sf::st_cast("MULTIPOLYGON"),
                                                      raster = dem,
                                                      field = "a_edge_ha")
uso_flo_unico_id_edge_raster
plot(uso_flo_unico_id_edge_raster)

#--- exportar raster
setwd("C:/Users/paulo/OneDrive - ufrpe.br/Documentos/UFPE/IMAT/ETNO/Mestrado/Disciplinas/Geoprocessamento/Git_Hub/disciplina-geoprocessamento/07_exercicios/exercicios_qgis/Dados_espaciais_no_R")
raster::writeRaster(x = uso_flo_unico_id_area_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_flo_unico_id_core_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_flo_unico_id_edge_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

# 48
#--- importar raster e arquivo DEM
st <- raster::stack(dem, uso_flo_unico_id_area_raster, uso_flo_unico_id_core_raster, uso_flo_unico_id_edge_raster)
st

names(st) <- c("dem", "flo", "core", "edge")
plot(st)

# 49
st_va <- st %>% #--- extrair valores compondo em um tibble
  raster::values() %>% 
  tibble::as_tibble()
st_va

#--- Relação entre as áreas de floresta e a elevação
# Floresta (total)
ggplot(data = st_va) +
  aes(x = dem, y = log10(flo + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

# àrea core
ggplot(data = st_va) +
  aes(x = dem, y = log10(core + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta core (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

# Área das bordas (edge)
ggplot(data = st_va) +
  aes(x = dem, y = log10(edge + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de borda de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

# Discussão: De acordo com a obseervação do gráfico e a linha de tendência, percebe-se que existe uma relação positiva forte entre altitude e as três áreas de florestas.
# Por ser na cidade do recife, isso pode estar associado ao fato de áreas mais distantes do centro apresentarem maior altitude e menor taxa de urbanização


# Fim ---------------------------------------------------------------------