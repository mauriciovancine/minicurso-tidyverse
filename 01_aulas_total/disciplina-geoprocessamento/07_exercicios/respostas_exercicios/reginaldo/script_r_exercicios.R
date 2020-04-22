# Wed Nov 13 14:44:03 2019 ------------------------------
# Objetivo: Exercicio disciplina geoprocessmento -UFRPE
# Aluno: Reginaldo Gusmão
# Atualização:

# Pacotes ------------------------------

library(tidyverse)
library(purrr)
library(ggplot2)
library(gridExtra)
library(sf)
library(raster)
library(fasterize)
library(ggpubr)

# Confira o diretorio ------------------------------
getwd()

# Limpando ------------------------------

rm(list = ls())

# Cálculos ------------------------------

# 1- Calcule |(2 * 5) - (3 ^ 2)|
(  2 * 5 ) - (3 ^ 2)

# 2- Calcule log10(10) + ln(100) * log2(1000)
log10(10) + log(100) * log2(1000)

#3- Calcule o fatorial de 10 (fa_10) e raiz quadrada (fa_10_rq)
fa_10 <- factorial(10)
fa_10_rq <- sqrt(fa_10)

#4- Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas
vel.med <- 400 / 3.5
vel.med

#5- Crie uma sequência unitária de 0 à 10
seq_10 <- seq(1:10)
seq_10_sum <- sum(seq_10)

#6- Crie uma sequência de 0 à 50, espaçada de 5 em 5.Repita os elementos desse objeto 10 vezes sequencialmente
seq_50 <- seq(0, 50, by = 5)
seq_50_rep_times <- rep(seq_50, 10)

#7- mega
mega <- list(NULL)
for (i in 1:8) {
  mega <- list(sample(1:60, 6))
  print(mega)
}

#8- Simule o resultado de 25 jogadas de um dado de 12 lados 
for (i in 1:25) {
  dado <- sample(1:12, 1)
  print(paste("Resultado da jogada", i, "foi", dado))
}

#9- Crie um vetor chamado "lo" para descrever 100 locais de amostragem. formato 1 = local_1

lo <- paste("local", 1:100, sep = "_")

#10- formato 2 = local_001  
lo <-
  c(
    paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = "")
  )

#14- Crie um fator chamado "tr", com dois níveis ("cont" e "trat").100 locais de amostragem, 50 de cada tratamento.

tr <- as.factor(c(rep("cont",50),rep("trat",50)))

#15- Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ma <- matrix(sample(0:10, 1000, replace = T), 100, byrow = F)

# Tidyverse ------------------------------

#16- use pipe max(log(exp(sqrt(rpois(100, 5)))))
rpois(100, 5) %>% sqrt() %>% exp() %>% log() %>% max()

#17- Remova %>% :rnorm(100) %>% exp() %>% log10() %>% min() %>%sqrt()
sqrt(min(log10(exp(rnorm(100)))))

#18- Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by() -> round(mean(sum(1:10)/3), digits = 1)
sum(1:10) %>% magrittr::divide_by(3) %>% mean() %>% round(1)

#19- Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
da2 <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

#20- glimpse()
glimpse(da)
glimpse(da2)

#21- Combine as colunas country, state, state_abbreviation, municipality, site, em uma coluna chamada local_total separadas por ,, atribuindo o resultado a um novo objeto

da3 <- da2 %>%
  unite(local_total,
        c(country, state, state_abbreviation, municipality, site),
        sep = ",")

#22- Separe a coluna passive_methods em outras colunas
da4 <- da3 %>% separate(passive_methods, c("pmchar1","pmchar2"))

#23- Retire as linhas com NA da coluna year_start
da5 <- da4 %>% drop_na(year_start)

#24- Selecione todas as colunas que contenham method
da6 <- da5 %>% select(ends_with("methods"))

#25- Faça um histograma da coluna species_number 
hist(da5$species_number)

#26- Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação log10 das colunas altitude, temperature e precipitation 
da5 <-
  da5 %>% mutate(alt_log = log(altitude),
                 tem_log = log(temperature),
                 pre_log = log(precipitation))

#27- Ordene os dados em forma decrescente pela coluna altitude
da7 <- da5 %>% arrange(desc(altitude))

#28- Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm
da8 <-
  da7 %>% filter(altitude > 1000,
                 temperature < 15 |
                   precipitation > 1000 &  precipitation <= 1500)

#29- Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies
sam <- da5 %>% filter(species_number > 15) %>% sample_n(200)

#30-Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation
range(da8$species_number, na.rm=TRUE)
range(da8$altitude, na.rm=TRUE)
range(da8$temperature,  na.rm=TRUE)
range(da8$temperature,  na.rm=TRUE)

#31-Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos.
dd_cor <- read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")
# esta funçã separa as colunas por ponto e virgula

#32- Gere gráficos relacionando o número de espécies e as variáveis altitude, temperature, precipitation. existe alguma relação?

g_alt <-
  ggplot(da8, aes(x = species_number, y = altitude)) + geom_point(size =
                                                                    2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Altitude") + theme_bw()

g_temp <-
  ggplot(da8, aes(x = species_number, y = temperature)) + geom_point(size =
                                                                       2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Temperatura") + theme_bw()

g_prec <-
  ggplot(da8, aes(x = species_number, y = precipitation)) + geom_point(size =
                                                                         2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Precipitation") + theme_bw()

grid.arrange(g_alt, g_temp, g_prec) # Baseado nos graficos provavelmente não há uma relação

#33-Gere gráficos  mostrando a frequência absoluta de amostragem em cada estado e exporte. (eixos invertidos)
names(da2)

dd_graf2 <-
  da2 %>% 
   dplyr::select("state_abbreviation", "effort_months", "species_number") %>%      na.omit() %>% group_by(state_abbreviation) %>% 
       summarise(freq_tt = sum(effort_months),                                                 numero_sp = sum(species_number))

ggplot(dd_graf2, aes(x = state_abbreviation, y = freq_tt)) + geom_bar(stat = "identity") + coord_flip()

ggsave("hist_state.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.pdf", wi=20,he = 15, un="cm", dpi=600)

#34- Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

ggbarplot(
  dd_graf2,
  x = "state_abbreviation",
  y = "numero_sp",
  fill = "state_abbreviation",
  color = "state_abbreviation",
  palette = c("Harmonic"),
  label = FALSE,
  lab.size = 5,
  xlab = "Estados",
  ylab = "Número de espécies",
  legend = "none",
  orientation = "horiz"
)

ggsave("hist_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_sp_ggpubr.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_sp_ggpubr.pdf", wi=20,he = 15, un="cm", dpi=600)

#35- Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

dd_pur <-
  da2 %>% dplyr::select("species_number", "coordinate_precision") %>% na.omit() %>% group_by(coordinate_precision) %>%
  summarise(numero_sp = sum(species_number))

ggbarplot(data=dd_pur,
          x = "coordinate_precision",
          y = "numero_sp", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          palette = c("PuOr"),
          label = FALSE, 
          lab.size = 5,
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none",
          order = c("gms","gm","dd","utm"))

ggsave("gps_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("gps_sp_ggpubr.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("gps_sp_ggpubr.pdf", wi=20,he = 15, un="cm", dpi=600)

# Qgis ------------------------------
# 36 a 46 no Qgis

# Geoprocessamento no R ------------------------------

# 47- Refaça os passos anteriores do QGIS (exercídios 37 a 46) utilizando as funções do pacote sf, raster e fasterize.
setwd("exercicios_qgis")

# Importar topologia (37)
rec_uso <- sf::st_read("PE_2611606_USO_corrigido_topologia.shp")

# Filtrar floresta (38)
floresta <- rec_uso %>% filter(CLASSE_USO == "formação florestal")

shapefile(floresta, "PE_2611606_USO_topologia.shp", overwrite=TRUE)

# Multipartes (40)
unico <- floresta %>% sf::st_cast("POLYGON")

# Criar coluna com (41)
unico2 <- unico %>% mutate(id = seq(1:nrow(unico)))

shapefile(unico2, "PE_2611606_USO_topologia_floresta_unico.shp", overwrite=TRUE)

# Selecionar apenas a coluna id (42)
unico3 <- unico2 %>% dplyr::select("id")

# Calcule a área (43)
unico4 <- unico3 %>% mutate(a_frag_ha = unico2$AREA_HA / 10000)

# Criando Buffer -60m (44)
core <-
  unico4 %>% sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)

shapefile(core, "PE_2611606_USO_topologia_floresta_unico_core60m.shp", overwrite=TRUE)

# Diferença simétrica (45)
uni_edg <-
  unico4 %>% sf::st_difference(sf::st_combine(core)) %>% mutate(a_edge_ha = sf::st_area(.) / 10000) 

shapefile(uni_edg, "PE_2611606_USO_topologia_floresta_unico_edge60m.shp", overwrite=TRUE)

# Vector para raster (46)
dem <- raster::raster("Mosaico_alinhado.tif")
dem

ras_unico4 <-
  fasterize::fasterize(
    sf = unico4,
    raster = dem,
    field = "a_frag_ha"
  )
names(ras_unico4) <- "area"
ras_unico4

ras_core <-
  fasterize::fasterize(
    sf = core %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = dem,
    field = "a_core_ha"
  )
names(ras_core) <- "core"
ras_core

ras_uni_edg <-
  fasterize::fasterize(
    sf = uni_edg %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = dem,
    field = "a_edge_ha"
  )
names(ras_uni_edg) <- "edge"
ras_uni_edg

# export
raster::writeRaster(x = ras_unico4, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    overwrite=TRUE)

raster::writeRaster(x = ras_core, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE",overwrite=TRUE)

raster::writeRaster(x = ras_uni_edg,
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE",overwrite=TRUE)

# 48- Importe os arquivos com o stack
st <- raster::stack(dem, ras_unico4, ras_core, ras_uni_edg)
names(st) <- c("dem", "flo", "core", "edge")
st

# 49- Extraia os valores desses quatro rasters, compondo os mesmos num tibble.Por fim, relacione esses valores das colunas (área, área core e área de borda - use o log10 para essas colunas, pois as unidades são diferentes) com o DEM. Faça gráficos de dispersão (scatterplot)

st_va <- extract(st) %>% as.tibble()
summary(st)

gf_flo <- ggplot(data = st_va, aes(x = dem, y = log10(flo))) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

gf_core <- ggplot(data = st_va, aes(x = dem, y = log10(core))) +
    geom_point() +
    labs(x = "Elevação (m)", y = "Área de floresta core (log10)") +
    stat_smooth(method = "lm") +
    theme_bw()
  
gf_edge <- ggplot(data = st_va, aes(x = dem, y = log10(edge + 1))) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de borda de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

grid.arrange(gf_flo,gf_core,gf_edge)# Existe uma relação positiva forte entre altitude e as áreas de florestas.Este padrão pode ter origem no fato que  áreas com maior altitude e menor taxa de urbanização por estarem distantes do centro.


