# Mon Nov 19 11:04:43 2019 ------------------------------
# Objetivo: Exercicio
# Paulo M. Sobrinho

# 3 Limpar memória
rm(list = ls())

# Pacotes

library(tidyverse)
library(gridExtra)
library(sf)
library(raster)
library(fasterize)
library(ggpubr)

# 4
(2*5) - (3^2)

# 5
log10(10) + log(100) * log2(1000)

# 6
fa_10 <- factorial(10)
fa_10_rq <- sqrt(fa_10)

# 7
400/3.5

# 8
seq_10 <- seq(1:10)
seq_10_sum <- sum(seq_10)

# 9
seq_50 <- seq(0,50,by = 5)
seq_50_rep_times <- rep(seq_50, 10)

# 10
numb <- list(NULL)
for (i in 1:8) {
  numb <- list(sample(1:60, 6))
  print(numb)
}

# 11
for (i in 1:25) {
  dado <- sample(1:12, 1)
  print(paste("Resultado", i, ":", dado))
}

# 12

lo <- paste("local", 1:100, sep = "_")
lo

# 13
lo2 <-
  c(
    paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = "")
  )

lo2

# 14 

tr <- as.factor(c(rep("cont",50),rep("trat",50)))
tr

# 15
ma <- matrix(sample(0:10, 1000, replace = T), 100, byrow = F)
ma

# 16 
rpois(100, 5) %>% sqrt() %>% exp() %>% log() %>% max()

# 17
sqrt(min(log10(exp(rnorm(100)))))

# 18
sum(1:10) %>% magrittr::divide_by(3) %>% mean() %>% round(1)

# 19
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

# 20 
glimpse(da)

# 21
da.un <- da %>% unite(local_total, c(country, state, state_abbreviation, municipality, site), sep = ",")
da.un$local_total

# 22
da.sep <- separate(da, passive_methods, sep = ",", into = NA)

# 23
da.na <- da %>% drop_na(year_start)

# 24
da.met <- da %>% select(ends_with("methods"))

# 25
qplot(da$species_number, geom = "histogram")

# 26
da.log <- da %>% mutate(alt_log = log10(altitude),
                 tem_log = log10(temperature),
                 pre_log = log10(precipitation)
                 )

# 27
da.ord <- da %>% arrange(desc(altitude))

# 28
da.sel <-
  da %>% filter(altitude > 1000,
                temperature < 15 | precipitation > 1000 &  precipitation <= 1500)

# 29
da.samp <- da %>% filter(species_number > 15) %>% sample_n(200)

# 30 (viajei na moral)
range(da$species_number, na.rm=TRUE)
range(da$altitude, na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)

# 31
da.co <- read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")
# Quando os dados estão separados por ponto e vírgula, devemos usar a função read_csv2.

# 32
alt <-
  ggplot(da,mapping=aes(x = species_number, y = altitude)) + 
  geom_point() + geom_smooth(method = "lm")
alt

ggsave(alt)

temp <-
  ggplot(da, mapping=aes(x = species_number, y = temperature)) +
  geom_point() + geom_smooth(method = "lm")
temp

ggsave(temp)

prec <-
  ggplot(da, mapping=aes(x = species_number, y = precipitation)) +
  geom_point() + geom_smooth(method = "lm")
prec

ggsave(prec)

# Embora seja necessário controlar outras variáveis como esforço amostral, existe uma tendência da riqueza ser maior com altitude e precipitação, e menor com a temperatura.

# 33
fa <- da %>% dplyr::select("state_abbreviation", "effort_months", "species_number") %>% na.omit() %>% group_by(state_abbreviation) %>% summarise(freq_tt = sum(effort_months), richness = sum(species_number))

ggplot(fa, aes(x = state_abbreviation, y = freq_tt)) + geom_bar(stat = "identity") + coord_flip()

ggsave("hist_state.png", wi=20,he = 15, un="cm", dpi=600)

# 34

ggbarplot(
  fa,
  x = "state_abbreviation",
  y = "richness",
  fill = "state_abbreviation",
  color = "state_abbreviation",
  palette = c("Harmonic"),
  label = FALSE,
  lab.size = 5,
  legend = "none",
  orientation = "horiz"
)

ggsave("hist_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)

#35 
gps <- da %>% dplyr::select("species_number", "coordinate_precision") %>% na.omit() %>% group_by(coordinate_precision) %>%  summarise(richness = sum(species_number))

ggbarplot(gps,
          x = "coordinate_precision",
          y = "richness", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          palette = c("PuOr"),
          label = FALSE, 
          lab.size = 5,
          legend = "none",
          order = c("gms","gm","dd","utm"))

ggsave("gps_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)

# 36-46 QGIS

# 47
setwd("exercicios_qgis")

# Importar shapefile de usos
usos <- sf::st_read("PE_2611606_USO_corrigido_topologia.shp", quiet = TRUE)


# Filtrar uso florestal
uso_flo <- usos %>% dplyr::filter(usos$CLASSE_USO == "formação florestal")

plot(uso_flo[5],col="green") #Checando

st_write(uso_flo,"PE_2611606_USO_topologia_floresta2.shp")# Salvando

# Simplificação
flo_uni<-sf::st_cast(uso_flo, "POLYGON")

plot(flo_uni[5],col="orange")

st_write(flo_uni,"PE_2611606_USO_topologia_floresta_unico2.shp")

# Criar coluna na tabela de atributos com o nome id
flo_uni2 <- flo_uni %>% mutate(id = seq(1:nrow(flo_uni)))

# Apagar outras colunas exceto id
flo_uni3 <- flo_uni2 %>% dplyr::select("id")

# Calcule a área (formato de coluna decimal) de cada fragmento em hectares na coluna a_frag_ha - 43

flo_uni4 <- flo_uni3 %>% mutate(a_frag_ha = sf::st_area(flo_uni3) / 10000)

# Criar Buffer negativo com calculo de área - 44

flo_buf <- flo_uni4 %>%
  sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)

flo_buf

plot(flo_buf)

st_write(flo_buf,"PE_2611606_USO_topologia_floresta_unico_core60m2.shp")

# Diferença simétrica - 45

flo_edg <-
  flo_uni4 %>% sf::st_difference(sf::st_combine(flo_buf)) %>% mutate(a_edge_ha = sf::st_area(.) / 10000)

plot(flo_edg[2],col="blue")

st_write(flo_edg,"PE_2611606_USO_topologia_floresta_unico_edge60m2.shp")

# Criando rasters de uso único, core e borda a partir de shapes e usando um rater como base - 46

molde <- raster::raster("Mosaico_alinhado.tif")
molde

flo_ras_unico4 <-fasterize::fasterize(sf = flo_uni4%>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"),raster = molde,field = "a_frag_ha")

plot(flo_ras_unico4)

writeRaster(flo_ras_unico4, "PE_2611606_USO_topologia_floresta_unico2",format = "GTiff",overwrite=TRUE)

flo_ras_core <-fasterize::fasterize(sf = flo_buf %>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"), raster = molde,field ="a_core_ha")

plot(flo_ras_core)

writeRaster(flo_ras_core, "PE_2611606_USO_topologia_floresta_unico_core60m2",format = "GTiff",overwrite=TRUE)

flo_ras_edg <-fasterize::fasterize(sf = flo_edg %>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"),raster = molde,field = "a_edge_ha")

plot(flo_ras_edg)

writeRaster(flo_ras_edg,"PE_2611606_USO_topologia_floresta_unico_edge60m2",format = "GTiff",overwrite=TRUE)


# 48
flo_stack<-stack(molde,flo_ras_unico4,flo_ras_core,flo_ras_edg)

plot(flo_stack)


# 49
names(flo_stack)<-c("molde","unico","core","borda")

st_ext <- flo_stack %>%
  raster::values()%>%
  tibble::as_tibble()

st_ext

gg_flo <- ggplot(data = st_ext)+
  aes(x = molde, y = log10(unico + 1)) +
  geom_point() +
  labs(x = "Elevação", y = "Área") +
  stat_smooth(method = "lm") +
  theme_bw()


gg_core <- ggplot(data = st_ext, aes(x = molde, y = log10(core + 1))) +
  geom_point() +
  labs(x = "Elevação", y = "Área core") +
  stat_smooth(method = "lm") +
  theme_bw()

gg_edge <- ggplot(data = st_ext, aes(x = molde, y = log10(borda + 1))) +
  geom_point() +
  labs(x = "Elevação", y = "Área de borda") +
  stat_smooth(method = "lm") +
  theme_bw()

grid.arrange(gg_flo,gg_core,gg_edge) # Há uma tendência a ter mais áreas florestadas nas regiões com maior altitude. Isso pode indicar uma tendência de desmatamento em áreas mais planas e de fácil acesso.