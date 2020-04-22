## 1. Criar um script
## 2. Criar um cabeçalho


# Exercicio disciplina Geoprocesamento
# Discente: Maria Julia Ferreira
# DATA: 21/10/2019

## 3. Removendo ojetos 
rm (list = ls ())
ls()

## 4.Calculos simples
cs = ((2 * 5) - (3 ^ 2))
cs

## 5. Mais calculos simples
cs2 <- (log10(10) + log(100)) * log2(1000) # função ln não é reconhecida, acredito que o certo seja usar log. 
cs2

## 6. Fatorial de 10:10

fac_10 <- factorial(10)
fac_10

fac_10_rq <- sqrt(fac_10) 
fac_10_rq

## 7. Velocidade média
vm <- 400/3.5
vm

## 8. sequencia unitária 
seq_10 <- 1:100
seq_10

seq_10_sum <- sum(seq_10, na.rm = FALSE)
seq_10_sum

## 9. Sequencia espaçada 
seq_50 <- seq(from = 0, to = 50, by = 5) 
seq_50

seq_50_rep_times <- rep(x = seq_50, times = 5)
seq_50_rep_times

## 10. Mega sena
numero_escolhido <- sample(1:60, 6, replace=FALSE)
numero_escolhido

lista_de_numeros <- list(numero_escolhido)
lista_de_numeros

rep_times <- rep(x = lista_de_numeros, times = 8)
rep_times

teste <- list(rep_times)
teste

## 11. Jogando RPG / Amostras aleatórias 
dado_rpg <- 1:12 
dado_rpg
jogadas = sample(dado_rpg, 25, replace = TRUE)
jogadas

## 12. local de amostragem
lo <- paste("local", 1:100, sep = "_")
lo

## 13. local de amostragem 2

library(stringr)
lo2 <- str_pad(number, 3, pad = "0")
lo2
lo3 <- paste("local", lo2, sep = "_")
lo3

## 14. Criar um fator com dois niveis
trat <- c("cont", "trat")
trat
tr <- as.factor(sort(rep.int(trat, 100)))
tr

## 15. Matriz de mil valores aleatorios entre 0 e 10

ma <- matrix(sample(0:10, 1000,replace=T), 100, ncol=10)
ma

## 16. Reescreva essa operação utilizando pipes %>%: max(log(exp(sqrt(rpois(100, 5)))))

library(tidyverse)

ma2 <- rpois(100, 5) %>% sqrt() %>%  exp() %>% max() %>% log()
ma2

##  17. Reescreva essa operação removendo os pipes %>%: rnorm(100) %>%
exp() %>%
  log10() %>% 
  min() %>%
  sqrt()

reescr <- sqrt(min(log10(exp(rnorm(100)))))
reescr

## 18. Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by()
round(mean(sum(1:10)/3), digits = 1)

library(magrittr)
reescr2 <- sum(1:10) %>% 
  mean() %>% 
  magrittr::divide_by(3) %>% 
  round(digits=1) 
reescr2

## 19. Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) com o tidyverse
getwd()
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
da
da2 <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da2

## 20. Utilize a função tibble::glimpse para verificar as colunas desses dados

tibble::glimpse(da)
tibble::glimpse(da2)

#Error in nchar(x) : invalid multibyte string, element 19 ### Não sei o que é????? 

## 21. Combine as colunas em uma coluna chamada local_total separadas por ,, 
## atribuindo o resultado a um novo objeto, utilizando o formato tidyverse 

library(tidyr)
da3<- da2 %>%
  unite(local_total,
        c(country, state, state_abbreviation, municipality, site),
        sep = ",")
da3
str(da3)

## 22. Separe a coluna passive_methods em outras colunas

da3$passive_methods
da4 <- da3 %>% separate(passive_methods, c("pmchar1","pmchar2"))
head(da4)

## 23. Retire as linhas com NA da coluna year_start, atribuindo o resultado a um novo objeto

da5<-da4 %>% drop_na(year_start)
da5

## 24. Selecione todas as colunas que contenham method, atribuindo o resultado a um novo objeto

names(da5)
da6 <- da5 %>% select(ends_with("methods"))
da6

## 25. Faça um histograma da coluna species_number utilizando o formato tidyverse

hist(da5$species_number)

## 26. Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação 
## log10 das colunas altitude, temperature e precipitation e atribua ao mesmo objeto da utilizando o formato tidyverse

da5 <-  da5 %>% mutate(alt_log = altitude,
                 tem_log = temperature,
                 pre_log = precipitation)
da5

## 27.  Ordene os dados em forma decrescente pela coluna altitude

da7 <- da5 %>% arrange(desc(altitude))
da7

## 28. Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm 
## e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto

da8 <- da7 %>% filter(altitude > 1000, temperature < 15 |
                   precipitation > 1000)
da8

## 29. Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies

aleat <- da5 %>% filter(species_number > 15) %>% sample_n(200)
aleat

## 30. Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr

calculo <- range(da8$species_number, na.rm=TRUE)
calculo
calculo1 <- range(da8$altitude, na.rm=TRUE)
calculo1
calculo2 <- range(da8$temperature,  na.rm=TRUE)
calculo2
calculo3 <- range(da8$temperature,  na.rm=TRUE)
calculo3 

## 31. Existem colunas com dados separados com vírgulas, o que gera problemas na leitura do CSV. 
## Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos
getwd()
dados_corrigidos <-  write_csv2(da2, "ATLANTIC_AMPHIBIANS_sites.csv", na = "NA", append = FALSE, col_names = TRUE,
                                quote_escape = "double")


## 32. Gere gráficos no ggplot2 relacionando o número de espécies e as variáveis altitude, temperature, precipitation, 
## exportando cada um deles, utilizando o formato tidyverse.

getwd()
setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia/graficos")

library(ggplot2)

#Altitude

ggplot(data = da2) +
  aes(x = altitude, y = species_number) +
  geom_point(colour = "black", fill = "blue", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Altitude", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

ggsave("spp_altitude.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) 

#Precipitação

ggplot(data = da2) +
  aes(x = precipitation, y = species_number) +
  geom_point(colour = "black", fill = "orange", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Precipitação", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

ggsave("spp_precipitacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

#Temperatura

ggplot(data = da2) +
  aes(x = temperature, y = species_number) +
  geom_point(colour = "black", fill = "green", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Temperatura", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("spp_temperatura.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

## Responda: existe alguma relação?
# Os graficos nos mostram que existem tendências no número de especies de amfibios. No caso o numero de espécies tende a aumentar 
# com a precipitação e a altitude; o inverso é observado com o aumento da temperatura. Os graficos nos mostram as tendencias 
# das relações estudadas, porém são necessarios testes para discutir sobre a significancia e a força destas relações. 

## 33. Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. 
## Inverta seu gráfico para a posição horizontal... e exporte utilizando o formato tidyverse

ta <- table(da2$state_abbreviation)
ta

ta_por <- ta %>% 
  as.data.frame
colnames(ta_por) <- c("state", "freq")
ta_por

ggplot(data = ta_por) +
  aes(x = state, y = freq) +
  geom_bar(color= ("black"), fill = ("#c51b8a"), stat = "identity") +
  coord_flip() +
  labs(x = "Estados",
       y = "Frequência de amostragem") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("state_freq.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

## 34. Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

library(ggpubr)

ggboxplot(data = da2, 
          x = "state_abbreviation", 
          y = "species_number",
          fill = c("#a6cee3", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
                   "#b15928","#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")

ggsave("ne_state.tiff", plot = last_plot(), device = tiff, width = 1000, height = 600, units = "in",
       dpi = 300, limitsize = FALSE)


ggbarplot(data=da2,
          x = "state_abbreviation",
          y = "species_number", 
          fill = "state_abbreviation", 
          color = "state_abbreviation",
          palette = c("#a6cee3", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
                      "#b15928","#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")+coord_flip()


ggsave("ne_state_bar.tiff", plot = last_plot(), device = tiff, width = 1000, height = 600, units = "in",
       dpi = 300, limitsize = FALSE)

## 35. Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

da_sem_na <- da2 %>% 
  drop_na("coordinate_precision")

da_sem_na$coordinate_precision


ggboxplot(data = da_sem_na, 
          x = "coordinate_precision", 
          y = "species_number",
          fill = "coordinate_precision",
          palette = c("#1b9e77, #7570b3, #e7298a, #66a61e"),
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none")


ggsave("ne_gps.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando


ggbarplot(data=da_sem_na,
          x = "coordinate_precision",
          y = "species_number", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          palette = c("#7fc97f","#beaed4","#fdc086","#ffff99"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")

ggsave("ne_gps_barplot.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando

###### Refazendo os passos anteriores do QGIS ######

library(tidyverse)
library (sf)
library(raster)
library(fasterize)

# 37.2. Importe o arquivo vetorial de uso e cobertura da terra que você rodou a topologia

getwd()
setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia")

rec_uso <- sf::st_read("PE_2611606_USO_correcao.shp")
rec_uso

plot(rec_uso[5] , main = NA, key.pos = NULL)

# 38.2. Selecione e exporte apenas a classe formação florestal da coluna CLASSE_USO
# criando o arquivo PE_2611606_USO_topologia_floresta.shp

rec_uso_floresta <- rec_uso %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")
rec_uso_floresta

plot(rec_uso_floresta[5], main=NA)

getwd()
setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia/vetores_novos")

sf::st_write(rec_uso_floresta,"PE_2611606_USO_topologia_floresta.shp")

# 39.2. + 40.2. Importe esse novo arquivo vetorial + Multipartes para partes simples

rec_uso_flor_un <- sf::st_cast(rec_uso_floresta,"POLYGON") 
rec_uso_flor_un

plot(rec_uso_flor_un[5], main=NA, key.pos = NULL) 

sf::st_write(rec_uso_flor_un,"PE_2611606_USO_topologia_floresta_unico.shp") 

# 41.2. Crie uma coluna (interger) na tabela de atributos com o nome id

rec_uso_flor_un_id <- rec_uso_flor_un %>% 
  dplyr::mutate(id=rec_uso_flor_un %>% nrow %>% seq()) 

rec_uso_flor_un_id

# 42.2. Apague as outras colunas, deixando apenas a coluna id

uso_id <- rec_uso_flor_un_id %>%
  dplyr::select(id)

uso_id

# 43.2. Calcule a área (formato de coluna decimal) de cada fragmento em hectares na coluna a_frag_ha

uso_id_a <- uso_id %>%
  dplyr::mutate(a_frag_ha= sf::st_area(geometry/10000))
uso_id_a

# 44.2. Faça um buffer com o valor negativo de 60 m, com o nome de PE_2611606_USO_topologia_floresta_unico_core60m.shp.

uso_buf <- uso_id %>% 
  sf::st_buffer(-60)
uso_buf

uso_id_a_core <- uso_buf %>%
  dplyr::mutate(a_core_ha=st_area(geometry)/10000) 
uso_id_a_core

plot(uso_id_a_core[3], main=NA, key.pos = NULL) 

sf::st_write(uso_buf,"PE_2611606_USO_topologia_floresta_unico_core60m.shp")

# 45.2. Calcular a área de borda desses fragmentos

uso_edge <- sf::st_difference(rec_uso_flor_un, sf::st_combine(uso_id_a_core))

uso_edge
plot(uso_edge[5], main = NA, key.pos =NULL)

uso_a_edge <- uso_edge%>%
  dplyr::mutate(a_ed60m_ha=st_area(geometry/1000)) 
uso_a_edge

sf::st_write(uso_a_edge,"PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

# 46.2. Converta esses três vetores para raster

getwd()
setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia/exercicios_qgis")
dir()
dem <- raster::raster("dem_mosaico_e_o_mundo2.tif")
dem

# Raster floresta unico
uso_id_a_raster <- fasterize::fasterize(sf = uso_id_a,
                                        raster = dem,
                                        field = "a_frag_ha")

uso_id_a_raster
plot(uso_id_a_raster)

# Raster core60m

uso_id_core_raster <- fasterize::fasterize(sf = uso_id_a_core %>%
                                             dplyr::filter(!st_is_empty(.)) %>% 
                                             sf::st_cast("MULTIPOLYGON"),
                                           raster = dem,
                                           field = "a_core_ha")
uso_id_core_raster
plot(uso_id_core_raster)

# Raster edge60m

uso_a_edge_raster <- fasterize::fasterize(sf = uso_a_edge %>%
                                            dplyr::filter(!st_is_empty(.)) %>% 
                                            sf::st_cast("MULTIPOLYGON"),
                                          raster = dem,
                                          field = "a_ed60m_ha")
uso_a_edge_raster
plot(uso_a_edge_raster)

# Salvando 

setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia/rasters_novos")

raster::writeRaster(x = uso_id_a_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_id_core_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_a_edge_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

# 48. Importe os arquivos raster gerados (com os valores de área, área core e área de borda) 
# junto com o arquivo de DEM usado anteriormente num stack raster::stack()

st <- raster::stack(dem, uso_id_a_raster, uso_id_core_raster, uso_a_edge_raster)
st

names(st) <- c("dem", "id", "core", "edge")
plot(st)

# 49. Extraia os valores desses quatro rasters, compondo os mesmos num tibble

st_va <- st %>% 
  raster::values() %>% 
  tibble::as_tibble()
st_va

# Por fim, relacione esses valores das colunas com o DEM
# Faça gráficos de dispersão


setwd("C:/Users/Julia/Documents/geopro/exercicio_final_Maria Julia/graficos")

# floresta

ggplot(data = st_va) +
  aes(x = dem, y = log10(id+1)) +
  geom_point(colour = "black", fill = "#7fc97f", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Elevação (m)", y = "Área de floresta (log10)") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("area_de_floresta_elevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

# core
ggplot(data = st_va) +
  aes(x = dem, y = log10(core+1)) +
  geom_point(colour = "black", fill = "steel blue", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Elevação (m)", y = "Área de floresta core (log10)") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("area_de_floresta_core_elevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

# edge

ggplot(data = st_va) +
  aes(x = dem, y = log10(edge+1)) +
  geom_point(colour = "black", fill = "#bebada", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Elevação (m)", y = "Área de borda de floresta (log10)") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("area_de_floresta_borda_elevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

# Há algum padrão? Discuta brevemente
# Os graficos apresentaram uma linha de tendencia positiva, mostrando que quando mais elevado, maior a cobertura florestal. 
# Porem quando olhamos para os pontos e sua distribuição (grafico: area_de_floresta_elevacao ),
# vemos que as áreas florestadas estão concentradas em regiões muito baixas ou muito altas; o que nós permite inferir que os locais 
# menos áreas de floresta  são aqueles na faixa de elevação intermediaria, ou seja, locais com maior urbanização em Recife. 

