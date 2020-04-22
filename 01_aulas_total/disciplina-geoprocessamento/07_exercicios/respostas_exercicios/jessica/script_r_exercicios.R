# -------------------------------------------------------------------------
# introdução ao geoprocessamento para etnobiologia e conservação da biodiversidade 

## exercicio


# jessica cunha
# 02-12-2019

#4
(2 * 5) - (3 ^ 2)

#5 log10(10) + ln(100) * log2(1000)
log10(10) + log(100) * log2(1000)

#6
factorial(10)
fa_10 <- factorial(10)
fa_10

fa_10_rq <- sqrt(fa_10)
fa_10_rq

#7 V=S/T
s <- 400
t <- 3.5
vel_m <- s/t

#8
seq_10 <- seq(0,10)
seq_10
seq_10_sum <- sum(seq_10)
seq_10_sum

#9
seq_50 <- seq(0,50, by=5)
seq_50_rep_times <- rep(x= 0:50, times=10)

#10
y <- replicate(8,sample(1:60,6,replace=FALSE))
dimnames(y) <- list(rownames(y,do.NULL=FALSE,prefix=""),
                    colnames(y,do.NULL=FALSE,prefix="Combination"))
m <- t(y)
(m2 <- t(apply(m,1,sort)))

#11
i <- 1
d <- list()
while(i < 26){
  d[[i]] <- sample(1:12,1)
  i <- i+1
}

#12
lo <- paste("local", 1:100, sep = "_")
lo

#13
lo <-
  c(
    paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = "")
  )
lo

#14

a <- rep("cont", 50)
b <- rep("trat", 50)
tr <- factor(x = c(a,b), levels = c("cont", "trat"))
tr

#15

ve <- sample(0:10, 10000, replace=TRUE)
ve
ma <- matrix(data=ve,nrow = 100, byrow = TRUE)
ma

#16
library(tidyverse)

rpois(100, 5)%>%
sqrt()%>%
exp()%>%
log()%>%
max()

#17
sqrt(min(log10(exp(rnorm(100))))) 

#18
library(magrittr)
install.packages("magrittr")

1:10 %>% 
  sum %>% 
  divide_by(3) %>% 
  round(digits = 1)

#19
setwd("C:/Users/Jessica/Desktop/Jessica/Doutorado/Disciplinas/Geo_etnobiologia/07_exercicios")
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

#20
tibble::glimpse(da)

#21
da_unite <- da %>% 
  unite("local_total", country, state, state_abbreviation, municipality, site, sep = ",")
da_unite

#22
da_separate <- da %>% 
  separate("passive_methods", c("pt", "dr"))
da_separate

#23
da_drop_na <- da %>% 
  drop_na("year_start")
da_drop_na

#24
da_select <- da %>% 
  select(active_methods, passive_methods, complementary_methods)
da_select

#25
hist(da$species_number,  xlab = "Sp",
     ylab = "Fr", main = "Species Number")

#26
da %>%
mutate(alt_log = log10(da$altitude),
       tem_log = log10(da$temperature),
       pre_log = log10(da$precipitation))  
da

#27
da_arrange_alt <- da %>% 
  arrange(desc(altitude))
da_arrange_alt

#28
da_filter <- da %>% 
  filter(altitude > 1000 | temperature < 15| precipitation > 1000 & precipitation < 1500 )
da_filter

#29
da_n <- da%>%
  filter(species_number > 15)%>%
  sample_n(200)
da_n

#30
drop_na_range <- da %>%
  select(species_number, altitude, temperature, precipitation) %>%
  drop_na()
drop_na_range

mean_var <- drop_na_range %>% 
  select(species_number, altitude,temperature, precipitation) %>% 
  map_dbl(mean)
mean_var

sd_var <- drop_na_range %>% 
  select(species_number, altitude,temperature, precipitation) %>% 
  map_dbl(sd)
sd_var

#31
dd_cor <- read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")
#Separar usando a função read_cvs2, ela separa as colunas por ponto e virgula


#32
# Espécies x altitude
plot(species_number ~ altitude,
     data = da,
     pch = 20,
     xlab = "Altitude",
     ylab = "Numero de espécies",
     cex.lab = 1.5,
     cex.axis = 1.3,
     bty = "l")
# Espécies x temperatura
plot(species_number ~ temperature,
     data = da,
     pch = 20,
     xlab = "Temperatura",
     ylab = "Numero de espécies",
     cex.lab = 1.5,
     cex.axis = 1.3,
     bty = "l")
# Espécies x chuva
plot(species_number ~ precipitation,
     data = da,
     pch = 20,
     xlab = "Precipitação",
     ylab = "Numero de espécies",
     cex.lab = 1.5,
     cex.axis = 1.3,
     bty = "l")

#33
library(ggplot2)
# frequence table
ta <- table(da$state_abbreviation)
ta <- round(ta/sum(ta) * 100, 2)
ta 

var <- data.frame(ta)

hp <- ggplot(data = var) +
  aes(x = var$Var1, y = var$Freq) +
  geom_violin(aes(fill = var$Var1), color = "black") +
  scale_fill_manual(values = c("blue", "forest green")) +
  geom_jitter(width = .3, alpha = .3) +
  labs(x = "Estado",
       y = "Frequencia") +
  theme_classic() +
  theme(axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
plot(hp)
hp + coord_flip()

ggsave("freq_state_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#34
library("ggpubr")
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

ggsave("spec_estado_ggpubr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)


#35
ggscatter(data = da,
          x = "coordinate_precision", 
          y = "species_number",
          color = "black",
          fill = "forestgreen",
          shape = 21, 
          size = 3,
          xlab = "Precisao GPS", 
          ylab = "Numero de especies")+
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15))

ggsave("spec_gps_ggpubr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#47
library(raster)
library(tidyverse)
library(fasterize)
library(sf)

setwd("C:/Users/Jessica/Desktop/Jessica/Doutorado/Disciplinas/Geo_etnobiologia/07_exercicios/exercicios_qgis")
 
##37
rec_uso <- sf::st_read("PE_2611606_USO_corrigido_topologia.shp")
dir.create("geo_r")
setwd("geo_r")

##38
uso_floresta <- rec_uso %>% 
dplyr::filter(CLASSE_USO == "formação florestal")
uso_floresta

plot(uso_floresta[1], main = NA)

##40
sf::st_write(uso_floresta, "PE_2611606_USO_topologia_floresta.shp")
uso_floresta_unico <- uso_floresta %>% sf::st_cast("POLYGON")

##41
sf::st_write(uso_floresta_unico, "PE_2611606_USO_topologia_floresta_unico.shp")
uso_floresta_unico2 <- uso_floresta_unico %>% mutate(id = seq(1:nrow(uso_floresta_unico)))

##42
uso_floresta_unico3 <- uso_floresta_unico2 %>% dplyr::select("id")

##43
uso_floresta_unico4 <- uso_floresta_unico3 %>% mutate(a_frag_ha = uso_floresta_unico2$AREA_HA / 10000)

##44
core <-
  uso_floresta_unico4 %>% sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)


##45
edge <-
  uso_floresta_unico4 %>% sf::st_difference(sf::st_combine(core)) %>% mutate(a_edge_ha = sf::st_area(.) / 10000)

##46 Vector para raster
dem <- raster::raster("Mosaico.tif")
dem


uso_unico_2 <-
  fasterize::fasterize(
    sf = uso_floresta_unico4,
    raster = dem,
    field = "a_frag_ha"
  )
names(uso_unico_2) <- "area"
uso_unico_2

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

uso_edge <-
  fasterize::fasterize(
    sf = edge %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = dem,
    field = "a_edge_ha"
  )
names(uso_edge) <- "edge"
uso_edge

## export
dir()
setwd("geo_R")
raster::writeRaster(x = uso_unico_2, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    overwrite=TRUE)

raster::writeRaster(x = uso_core, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE",overwrite=TRUE)

raster::writeRaster(x = uso_edge,
                   filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                   format = "GTiff",
                   options= "COMPRESS=DEFLATE",overwrite=TRUE)

#48
st <- raster::stack(dem, uso_unico_2, uso_core, uso_edge)
names(st) <- c("dem", "flo", "core", "edge")
st


#49
st_va <- raster::extract(st) %>% as.tibble()
summary(st)

gg_flo <- ggplot(data = st_va, aes(x = dem, y = log10(flo))) +
  geom_point() +
  labs(x = "Elevacao (m)", y = "Area de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

gg_core <- ggplot(data = st_va, aes(x = dem, y = log10(core))) +
  geom_point() +
  labs(x = "Elevacao (m)", y = "Area de floresta core (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

gg_edge <- ggplot(data = st_va, aes(x = dem, y = log10(edge + 1))) +
  geom_point() +
  labs(x = "Elevacao (m)", y = "Area de borda de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

#Houve relação.
#Verifica-se que baixas altitudes facilita o processo de substituição das áreas florestais.
#Ou seja, há uma modificação da paisagem florestal em relação ao gradiente altitudinal.
#Quanto maior a altitude, maior cobertura florestal. 