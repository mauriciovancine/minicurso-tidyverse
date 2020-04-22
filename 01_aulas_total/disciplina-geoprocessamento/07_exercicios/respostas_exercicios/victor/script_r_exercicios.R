# --------------------------------------------
# --- Discentes: Victor Leandro Silva
# --- Data: 18 - 11 - 2019
# --- Objetivo: 
# --------------------------------------------

# Pacotes ------------------------------

library(tidyverse)
library(purrr)
library(ggplot2)
library(gridExtra)
library(sf)
library(raster)
library(fasterize)
library(ggpubr)

# -------------------------------------------- 
# memory environment
rm(list = ls())

# --------------------------------------------
(2 * 5) - (3 ^ 2)

# --------------------------------------------  
log10(10) + log(100) * log2(1000)

# --------------------------------------------  
fa_10 <- factorial(10)

# --------------------------------------------  
fa_10_rq <- sqrt(fa_10)

# -------------------------------------------- 
S <- 400 # km/h
t <- 3.5 # horas
Vel_m <- S/t

# --------------------------------------------
seq_10 <- 0:10
seq_10_sum <- sum(seq_10)

# --------------------------------------------
seq_50 <- seq(0,50, by=5)
seq_50_rep_times <- rep(0:50, times=10)

# --------------------------------------------
# 10
i <- 1
y <- list()
while(i < 9){
y[[i]] <- sample(1:60,6)
i <- i+1
}

# --------------------------------------------
i <- 1
d <- list()
while(i < 26){
  d[[i]] <- sample(1:12,1)
  i <- i+1
}

# --------------------------------------------
lo <- paste("Local", 1:100, sep = "_")
lo

# --------------------------------------------
# 13
lo <-
  c(
    paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = "")
  )

# --------------------------------------------
a <- rep("cont",50)
b <- rep("trat",50)
tr <- factor(x = c(a,b), levels = c("cont","trat"))
tr

# --------------------------------------------
v <- sample(0:10, 10000, replace = T)
ma <- matrix(data = v, nrow = 100, ncol = 100)
ma

# --------------------------------------------
rpois(100,5)%>%
  sqrt()%>%
  exp()%>%
  log()%>%
  max()

# --------------------------------------------
sqrt(min(log10(exp(rnorm(100)))))

# --------------------------------------------
1:10 %>% 
  sum %>% 
  divide_by(3) %>% 
  round(digits = 1)

# --------------------------------------------
setwd("C:/Users/Victor Leandro/Documents/exercicios geoprocessament/07_exercicios")
getwd()
dir()

da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da_s <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

# --------------------------------------------
#20

tibble::glimpse(da)
tibble::glimpse(da_s)

# --------------------------------------------
da_unite <- da %>% 
  unite("local_total", country,state,state_abbreviation,municipality,site, sep = ",")
da_unite

# --------------------------------------------
da_separate <- da %>% 
  separate("passive_methods", c("pt","dr"))
da_separate

# --------------------------------------------
drop_na <- da %>% 
  drop_na("year_start")
drop_na

# --------------------------------------------
da_select <- da %>% 
  select(active_methods, passive_methods, complementary_methods)
da_select

# --------------------------------------------
hist(da$species_number, main = "Jessica Cunha")

# -------------------------------------------
da %>% 
  mutate(alt_log = log10(da$altitude),
         tem_log = log10(da$temperature),
         pre_log = log10(da$precipitation))
da

# -------------------------------------------
da_arrange_alt <- da %>% 
  arrange(desc(altitude))
da_arrange_alt

# -------------------------------------------
da_filter <- da %>% 
  filter(altitude > 1000 | temperature < 15 | precipitation > 1000 & precipitation < 1500)
da_filter

# -------------------------------------------
da_n <- da %>% 
  filter(species_number > 15)%>%
  sample_n(200)
da_n

# -------------------------------------------
#30

drop_na_range <- da %>% 
  select(species_number, altitude, temperature, precipitation)%>%
  drop_na()
drop_na_range
#média
mean_var <- drop_na_range %>% 
  select(species_number, altitude, temperature, precipitation) %>% 
  map_dbl(mean)
mean_var

# calcular o desvio padrao para varias colunas
sd_var <- drop_na_range %>% 
  select(species_number, altitude, temperature, precipitation) %>% 
  map_dbl(sd)
sd_var
# -------------------------------------------
#31
da_cor <- read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")
#Separar usando a função read_cvs2, ela separa as colunas por ponto e virgula

# -------------------------------------------
#32
# Espécies x altitude
g_alt <-
  ggplot(da8, aes(x = species_number, y = altitude)) + geom_point(size =
                                                                    2.5) + geom_smooth(method = "lm") + xlab("Species number") + ylab("Altitude") + theme_bw()

g_temp <-
  ggplot(da8, aes(x = species_number, y = temperature)) + geom_point(size =
                                                                       2.5) + geom_smooth(method = "lm") + xlab("Species number") + ylab("Temperatura") + theme_bw()

g_prec <-
  ggplot(da8, aes(x = species_number, y = precipitation)) + geom_point(size =
                                                                         2.5) + geom_smooth(method = "lm") + xlab("Species number") + ylab("Precipitation") + theme_bw()

grid.arrange(g_alt, g_temp, g_prec)
# -------------------------------------------
#33
# frequence table
ta <- table(da$state_abbreviation)
ta <- round(ta/sum(ta) * 100, 2)
ta 

var <- data.frame(ta)

hp <- ggplot(data = ta) +
  aes(ta$Var1, y = ta$Freq) +
  geom_violin(aes(fill = ta$Var1), color = "black") +
  scale_fill_manual(values = c("blue", "forest green")) +
  geom_jitter(width = .3, alpha = .3) +
  labs(x = "Tipo de registro",
       y = "NÃºmero de espÃ©cies") +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
plot(hp)

hp + coord_flip()
ggsave("freq_estador.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# -------------------------------------------
# ggpubr
ggscatter(data = da,
          x = "state_abbreviation", 
          y = "species_number",
          color = "black",
          fill = "forestgreen",
          shape = 21, 
          size = 3,
          xlab = "Estados", 
          ylab = "Numero de especies")+
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

ggsave("spec_estador.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# -------------------------------------------
# ggpubr
ggscatter(data = da,
          x = "coordinate_precision", 
          y = "species_number",
          color = "black",
          fill = "forestgreen",
          shape = 21, 
          size = 3,
          xlab = "Precisao do GPS", 
          ylab = "Numero de especies")+
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

ggsave("spec_gpsr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

 
#47 -------------------------------------------
setwd("C:/Users/Victor Leandro/Documents/exercicios geoprocessament/07_exercicios/exercicios_qgis")
getwd()
dir()

# Refazer as atividades de antes no R


# 37
rec_uso <- sf::st_read("PE_2611606_USO_corrigido_topologia.shp")

# 38
floresta <- rec_uso %>% filter(CLASSE_USO == "formação florestal")

# 40
unico <- floresta %>% sf::st_cast("POLYGON")

# 41
unico2 <- unico %>% mutate(id = seq(1:nrow(unico)))

# 42
unico3 <- unico2 %>% dplyr::select("id")

# 43
unico4 <- unico3 %>% mutate(a_frag_ha = unico2$AREA_HA / 10000)

# 44
core <-
  unico4 %>% sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)

# 45
uni_edg <-
  unico4 %>% sf::st_difference(sf::st_combine(core)) %>% mutate(a_edge_ha = sf::st_area(.) / 10000) 

# Vector para raster (46)
dem <- raster::raster("Mosaico.tif")
dem

use_unico_2 <-
  fasterize::fasterize(
    sf = unico4,
    raster = dem,
    field = "a_frag_ha"
  )
names(use_unico_2) <- "area"
use_unico_2

uso_core <-
  fasterize::fasterize(
    sf = core %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = dem,
    field = "a_core_ha"
  )
names(uso_core) <- "core"
uso_core

uso_edg <-
  fasterize::fasterize(
    sf = uni_edg %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = dem,
    field = "a_edge_ha"
  )
names(uso_edg) <- "edge"
uso_edg

# export
dir()
setwd("geo_R")
raster::writeRaster(x = use_unico_2, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    overwrite=TRUE)

raster::writeRaster(x = uso_core, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE",overwrite=TRUE)

raster::writeRaster(x = uso_edg,
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE",overwrite=TRUE)

# 48

setwd("C:/Users/Victor Leandro/Documents/exercicios geoprocessament/07_exercicios/exercicios_qgis")
dir()
li <- dir(pattern = "tif")
st <- raster::stack(li)
## Ou
st <- raster::stack(dem, use_unico_2, uso_core, uso_edg)
names(st) <- c("dem", "flo", "core", "edge")
st

# 49
st_va <- raster::extract(st) %>% as.tibble()
summary(st)

gf_flo <- ggplot(data = st_va, aes(x = dem, y = log10(flo))) +
  geom_point() +
  labs(x = "Altitude (m)", y = "Area Floresta") +
  stat_smooth(method = "lm") +
  theme_bw()

gf_core <- ggplot(data = st_va, aes(x = dem, y = log10(core))) +
  geom_point() +
  labs(x = "Elevacao (m)", y = "Area Floresta") +
  stat_smooth(method = "lm") +
  theme_bw()

gf_edge <- ggplot(data = st_va, aes(x = dem, y = log10(edge + 1))) +
  geom_point() +
  labs(x = "Elevacao (m)", y = "Area Floresta") +
  stat_smooth(method = "lm") +
  theme_bw()

grid.arrange(gf_flo,gf_core,gf_edge)
# Existem uma relacao entre as areas com maior floresta e com a altiture
# onde as áreas de maior altitude possuem mais áreas florestais
