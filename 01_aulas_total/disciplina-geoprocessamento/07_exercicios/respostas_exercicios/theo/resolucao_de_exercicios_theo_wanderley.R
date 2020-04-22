#############################
#Script com resolução de problemas da disciplina "Introdução ao 
#geoprocessamento para Etnobiologia e Conservação da Biodiversidade - PPGETNO."
#Discente Theo Branco de Oliveira Wanderley
##02/12/2019##

# Pacotes ####
library(purrr)
library(tidyverse)
library(purrr)
library(ggplot2)
library(GGally)
library(svglite)
library(ggpubr)
library(sf)
library(rgdal)
library(gridExtra)

#Exercícios####
# 3 - Limpar ambiente

rm(list = ls())

# 4 Vamos começar com alguns cálculos simples:

(2 * 5) - (3 ^ 2)

# 5 Mais alguns cálculos simples:

log10(10) + ln(100) * log2(1000)

# 6 Ainda mais alguns cálculos simples:

fa_10<-factorial(10)# Fatorial
fa_10

fa_10_rq<-sqrt(fa_10)# Raiz quadrada
fa_10_rq

# 7 Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas
#VM=VS/VT
400/3.5

# 8
# Crie uma sequência unitária de 0 à 10
seq_10<-0:10 
seq_10

# Some os elementos desse objeto cumulativamente
seq_10_sum<-sum(seq_10) 
seq_10_sum

# 9
# Crie uma sequência de 0 à 50, espaçada de 5 em 5
seq_50 <- seq(from = 0, to = 50, by = 5)seq_50

# Repita os elementos desse objeto 10 vezes sequencialmente
seq_50_rep_times<-rep(seq_50, times = 10) seq_50_rep_times

# 10 Escolha 6 números para jogar na Mega-Sena durante um mês (duas vezes por semana)
mega_num <- list(NULL)
for (i in 1:8) {
  mega_num <- list(sample(1:60, 6))
  print(mega_num)
}

# 11 Simule o resultado de 25 jogadas de um dado de 12 lados

rpg<-sample(1:12, 25, replace = T)
rpg

# 12 Crie um vetor chamado "lo" para descrever 100 locais de amostragem

lo <- paste("local", 1:100, sep = "_")
lo

# 13 Crie um vetor chamado "lo" para descrever 100 locais de amostragem

lo <- paste("local", 1:100, sep = "_00")
lo

#14 Crie um fator chamado "tr", com dois níveis ("cont" e "trat") para descrever 100 locais de amostragem, 50 de cada tratamento

tr <- rep(x = c("cont", "trat"), each = 50)
tr <- as.factor(tr)
levels(tr)
#15 Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ve<-sample(0:10,10000,replace = T)
ma <- matrix(data = ve, nrow = 100, ncol = 100, byrow = FALSE)
ma

# 16 Reescreva essa operação utilizando pipes %>%

rpois(100, 5) %>%
sqrt() %>%
exp() %>%
log() %>%
max()
  

# 17 Reescreva essa operação removendo os pipes %>%

obj <- sqrt(min(log10(exp(rnorm(100)))))
obj

# 18 Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by()
#round(mean(sum(1:10)/3), digits = 1)

sum(1:10)%>%
  magrittr::divide_by(3) %>%
  mean()%>%
  round(1)


# 19 Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse

da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

# 20 Utilize a função tibble::glimpse para verificar as colunas desses dados

tibble::glimpse(da)

# 21 Combine as colunas country, state, state_abbreviation, municipality, site, em uma coluna chamada local_total separadas por ,, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_comb <- da %>%
  unite("local_total", country:site, sep = ",")
da_comb$local_total

# 22 Separe a coluna passive_methods em outras colunas (mesmo com o erro...), atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_new <- da %>%
  separate("passive_methods", c("ar", "dr", "ft", "pt"), remove = FALSE)
da_new

# 23 Retire as linhas com NA da coluna year_start, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_na <- da %>%
  drop_na(year_start)
da_na

# 24 Selecione todas as colunas que contenham method, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_select <- da %>%
  select(contains("method"))
da_select

# 25 Faça um histograma da coluna species_number utilizando o formato tidyverse

hist(da$species_number)

# 26 Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação log10 das colunas altitude, temperature e precipitation e atribua ao mesmo objeto da utilizando o formato tidyverse

da <- da %>%  mutate(alt_log = log10(altitude),tem_log = log10(temperature),pre_log = log10(precipitation))
da

# 27 Ordene os dados em forma decrescente pela coluna altitude, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_ord <- da %>% arrange(desc(altitude))
da_ord

# 28 Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_filter <- da %>%
  filter(altitude > 1000, temperature < 15 |precipitation >1000 | precipitation <=1500)
da_filter

# 29 Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_sam <- da %>% filter(species_number > 15) %>% sample_n(200)
da_sam

# 30 Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr utilizando o formato tidyverse
#usando purrr
range(da$species_number, na.rm=TRUE)
range(da$altitude, na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)

# 31 Cometi um grave erro no data paper de anfíbios... Algo relacionado à colunas com dados separados 
#com vírgulas, aí quando se abre num .csv (separado por vírgulas) dá ruim... 
#Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos


# O problema pode ter sido devido ao separador padrão utilizado ao salvar o arquivo
# já que alguns programas salvam o formato .csv, com separação por ";".
# Desta forma a melhor solução é especificar o argunto "sep", descrendo o separador como ";",
# ex.: read.csv(******.csv, sep";")
#
#Os dados estração errados se não for especificado o separador correto,
#pois não será feira a distribuição das colunas de forma correta.

# 32 Gere gráficos no ggplot2 relacionando o número de espécies e as variáveis altitude, 
#temperature, precipitation, exportando cada um deles, utilizando o formato tidyverse. Responda: existe alguma relação?

library(corrplot)

names(da)
# altitude

g_alt <-
  ggplot(da, aes(y = species_number, x = altitude)) + geom_point(size = 2.5) + geom_smooth(method = "lm") +
  xlab("Número de espécies") + ylab("Altitude") + theme_bw() + geom_tile() +
  ggsave("da_spp_alt.jpg", height = 10, width = 10, dpi = 150, units = "cm") 
g_alt

# Existe relação?

alt.data <- da[,c(3,23)]
cor(alt.data)
#Existe uma correlação positiva muito baixa (0.11)

 # temperatura
g_temp <-
  ggplot(da, aes(y = species_number, x = temperature)) + geom_point(size = 2.5) + geom_smooth(method = "lm") + ylab("Número de espécies") + xlab("Temperatura") + theme_bw() +
ggsave("da_spp_temp.jpg", height = 10, width = 10, dpi=150, units = "cm")
g_temp

# Existe relação?
temp.data <- da[,c(3,24)]
cor(na.omit(temp.data))
#Existe uma correlação negativa muito baixa (-0.13)


# precipitação
g_prec <-
  ggplot(da, aes(y = species_number, x = precipitation)) + geom_point(size = 2.5) + geom_smooth(method = "lm") + ylab("Número de espécies") + xlab("Precipitation") + theme_bw() +
  ggsave("da_spp_temp.jpg", height = 10, width = 10, dpi=150, units = "cm")
g_prec

# Existe relação?
prec.data <- da[,c(3,25)]
cor(na.omit(prec.data))
#Existe uma correlação positiva muito baixa (0.13)


# 33 Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. 
#Ah, inverta seu gráfico para a posição horizontal... e exporte utilizando o formato tidyverse

da_fa <-da %>% select("state", "effort_months") %>% na.omit() %>% group_by(state) %>% summarise(freq_tt = sum(effort_months))

g4 <- ggplot(da_fa, aes(x = state, y = freq_tt)) + geom_bar(stat="identity")+ scale_fill_grey()+
  coord_flip() + ggsave("da_fa.svg", height = 14, width = 18, dpi=300, units = "cm")

g4

# 34 Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

da_fa2<-as.data.frame(da)
da_fa2$state2<-as.factor(da_fa2$state2)#Transformando os estados em fatores

str(da_fa2)

ggbarplot(data = da_fa2,
            y = "species_number", x = "state_abbreviation",
            fill = "state_abbreviation",
            xlab = "Estado (Abreviação)",
            ylab = "Número de espécies",
            add = "mean") + ggsave("ggpubr_fa_state.jpg", width = 10, height = 10, units = "cm", dpi = 300)

# 35 Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte
str(da_fa2)
da_fa2$coordinate_precision2<-as.factor(da_fa2$coordinate_precision)# Transformando em fator
ggbarplot(data = da_fa2,
            x = "coordinate_precision2", y = "species_number",
            add = "median",
            fill = "coordinate_precision2",
            xlab = "Gps - Precisão",
            ylab = "Número de espécies")
ggsave("ggpubr_fa_precision.jpg", width = 10, height = 10, units = "cm", dpi = 300)


# 36 até 46 no Qgis.

# 47- Refaça os passos anteriores do QGIS (exercídios 37 a 46) utilizando as funções do pacote sf, raster e fasterize.
#Meu jézuis

# 37  Importar topologia 
rec_uso <- sf::st_read("PE_2611606_USO_topologia.shp")
st_transform(rec_uso, CRS)
# 38 Filtrar floresta
floresta <- rec_uso %>% filter(CLASSE_USO == "formação florestal")
st_write(floresta, "PE_2611606_USO_topologia_r.shp")

#39....

# 40  Multipartes 
unico_shape <- floresta %>% sf::st_cast("POLYGON")

# 41 Coluna ID
unico2 <- unico %>% mutate(id = seq(1:nrow(unico)))

st_write(unico2, "PE_2611606_USO_topologia_floresta_unico_r.shp")

# 42 Selecionar col id 
unico3 <- unico2 %>% dplyr::select("id")

# 43 Calcule a área em id 
unico4 <- unico3 %>% mutate(a_frag_ha = unico2$AREA_HA / 10000)

# 44 Fazendo Buffer - 60 metros
core <- unico4 %>% sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)
st_write(core, "PE_2611606_USO_topologia_floresta_unico_core60m_r.shp")

# 45 Diferença simétrica
uni_edg <-  unico4 %>% sf::st_difference(sf::st_combine(core)) %>%
  mutate(a_edge_ha = sf::st_area(.) / 10000) 
st_write(uni_edg, "PE_2611606_USO_topologia_floresta_unico_edge60m_r.shp")

# Transformando em raster (.tif)
rec_mum <- raster::raster("dem_mosaico_recife_mundo.tif")
rec_mum

ras_unico4 <-
  fasterize::fasterize(
    sf = unico4,
    raster = rec_mum,
    field = "a_frag_ha"
  )
names(ras_unico4) <- "area"
ras_unico4

ras_core <-
  fasterize::fasterize(
    sf = core %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = rec_mum,
    field = "a_core_ha"
  )
names(ras_core) <- "core"
ras_core

ras_uni_edg <-
  fasterize::fasterize(
    sf = uni_edg %>%
      dplyr::filter(!st_is_empty(.)) %>%
      sf::st_cast("MULTIPOLYGON"),
    raster = rec_mum,
    field = "a_edge_ha"
  )
names(ras_uni_edg) <- "edge"
ras_uni_edg

# exportando

raster::writeRaster(x = ras_unico4, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_r",
                    format = "GTiff")

raster::writeRaster(x = ras_core, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m_r",
                    format = "GTiff")

raster::writeRaster(x = ras_uni_edg,
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m_r",
                    format = "GTiff")

# 48 Importe os arquivos com o stack
st <- raster::stack(rec_mum, ras_unico4, ras_core, ras_uni_edg)
names(st) <- c("dem", "flo", "core", "edge")
st

# 49 Extraia os valores desses quatro rasters em um tibble.

st_va <- extract(st) %>% as_tibble()
summary(st_va)

gf_flo <- ggplot(data = st_va, aes(x = dem, y = log10(flo))) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta (log10)")+
  stat_smooth(method = "lm")

gf_core <- ggplot(data = st_va, aes(x = dem, y = log10(core))) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta core (log10)") +
  stat_smooth(method = "lm")

gf_edge <- ggplot(data = st_va, aes(x = dem, y = log10(edge + 1))) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de borda da floresta (log10)") +
  stat_smooth(method = "lm")

grid.arrange(gf_flo,gf_core,gf_edge)

#Há algum padrão? Discuta brevemente

#Existe um padrão ao que se refere á "área core da floresta(log)" e 
#à "área de borda da floresta(log)", em relação à elevação.
# Possivelmente esta relação existe pelo fato de regiões mais altas,
#como o alto dos morros, serem automáticamente enquadradas como "áreas de -
#preservação permanente (APP)". Desta forma naturalmente existirão mais florestas
#e maiores áreas com este tipo de solo em regiões mais elevadas.
