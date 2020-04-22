#Atividade Geoprocessamento
#06 11 2019
#Kamia Marques Pedrosa

setwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento")


#4.Vamos começar com alguns calculos simples
2 * 5 - 3 ^ 2

#5.Mais alguns cálculos simples

log10(10) + log(100) * log2(1000)
10:10

#6. Ainda mais alguns cálculos simples:

"fa_10"
"fa_10_rq"
lo <- log(10)
lo

#7. Sim, mais alguns cálculos simples:

400/3.5

#8.bora de sequências

obj_10 <- 10
"seq_10_sum"
obj_50 <-50

#9.Agora com repetições

"seq_50_rep_times"
obj_60 <-10
"seq_60_rep_times"
2*30
obj_60 <-6
25*12

#10. Escolha 6 números para jogar na Mega-Sena durante um mês (duas vezes por semana)
#Atribua esses resultados à uma lista, de modo que cada elemento contenha os 6 números
#Lembrando: valores da Mega-Sena vão de 1 a 60

mega<-sample(1:60,6)
mega

jogo1<-sample(mega)
jogo2<-sample(mega)
jogo3<-sample(mega)
jogo4<-sample(mega)
jogo5<-sample(mega)
jogo6<-sample(mega)
jogo7<-sample(mega)
jogo8<-sample(mega)

#11.vetores (lista com oito jogos)
li<-list(jogo1,jogo2,jogo3,jogo4,jogo5,jogo6,jogo7,jogo8)

#11.simule o resultado de 25 jogadas de um dado de 12 lados
dado<-sample (1:12:25)

#12.Crie um vetor chamado "lo" para descrever 100 locais de amostragem. O vetor deve ter esse formato:
lo<-c("local_1, local_2, local_3, ...., local_100")
lo<-c("local_001, local_002, local_003, ...., local_100")
tr<-C()

# 14. Crie um fator chamado "tr", com dois níveis ("cont" e "trat") para descrever 100 locais de amostragem, 50 de cada tratamento. 
# O fator deve ter esse formato:cont, cont, cont, ...., cont, trat, trat, trat, ...., trat

library(tidyverse)
tr <- rep(x = c("cont","trat"), each=50) %>%
  factor(c("cont", "trat"),
         levels = c("cont", "trat")) 
tr


#15.Crie uma matrix chamada "MA" da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ve <- c(1, 2, 3, 4,5,6,7,8,9,10)
ma<-matrix(data=ve, nrow=100, ncol=100, byrow=TRUE)

#16. Reescreva essa operação utilizando pipes %>%:max(log(exp(sqrt(rpois(100, 5)))))

install.packages("magrittr")
library(magrittr)
max(log(exp(sqrt(rpois(100, 5)))))
x <- c(1,2,3,4,5)
x %>% sum %>% sqrt
T %>% max(c(NA, rnorm(100)), na.rm = 5) 
t

#17.Reescreva essa operação removendo os pipes %>%:
max(log(exp(sqrt(min(rnorm(100,10))))))

#18. Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by()

sum(1:10) %>%
magrittr::divide_by(3)%>%#com pipes
mean() %>%
round(digits=1)

#19. Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse
setwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/03_dados/00_tabelas")
read.csv("ATLANTIC_AMPHIBIANS_sites.csv")
da <- read.csv("ATLANTIC_AMPHIBIANS_sites.csv")

#20. Utilize a função tibble::glimpse para verificar as colunas desses dados

tibble::glimpse(da)


#21.Combine as colunas country, state, state_abbreviation, municipality, site, em uma coluna chamada local_total separadas por ,, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse
install.packages("tidyverse")
library("tidyverse")

da_uni <- da %>% 
  unite("local_total", country, state, state_abbreviation, municipality, site, sep=",") #atribuindo um novo objeto e unindo as colunas
da_uni$local_total 


#22. Separe a coluna passive_methods em outras colunas (mesmo com o erro...), atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

si_separate <- da %>% 
  separate("passive_methods", c("mo", "da", "tw", "ni"), remove = FALSE)
si_separate[, c(1, 9:13)]

#23.Retire as linhas com NA da coluna year_start, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse
da_drop_na <- da %>% 
  drop_na(year_start)
da_drop_na

#24.Selecione todas as colunas que contenham method, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse
da_select <- da %>% 
  select(contains("method"))
da_select

#25. Faça um histograma da coluna species_number utilizando o formato tidyverse

ggplot(data = da) +
  aes(species_number) +
  geom_histogram(fill= "#e34a33", color="#e9ecef")

#26. Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação log10 das colunas altitude, temperature e precipitation e atribua ao mesmo objeto da utilizando o formato tidyverse

da_mutate <- da %>% 
  mutate(alt_log= log(altitude), tem_log= log(temperature), pre_log= log(precipitation))
da_mutate

#27. Ordene os dados em forma decrescente pela coluna altitude, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_arrange <- da %>% 
  arrange(altitude)
da_arrange

#28. Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_filter <- da %>% 
  filter(altitude > 1000 & temperature < 15 | precipitation  >1000 & precipitation <= 1500 ) 
da_filter

#29. Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_sample_n <- da %>% 
  filter(species_number>15) %>% 
  sample_n(200)
da_sample_n

#30.Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr utilizando o formato tidyverse

library("purrr")

da_range <- da %>%
  select(species_number, altitude, temperature, precipitation) %>% 
  map(range, na.rm=TRUE)
da_range

#31. Cometi um grave erro no data paper de anfíbios... Algo relacionado à colunas com dados separados com vírgulas, aí quando se abre num .csv (separado por vírgulas) dá ruim... Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos
#????

#32. Gere gráficos no ggplot2 relacionando o número de espécies e as variáveis altitude, temperature, precipitation, exportando cada um deles, utilizando o formato tidyverse. Responda: existe alguma relação?

library(ggplot2)

#Número de espécies e Altitude

ggplot(data = da) +
  aes(x = altitude, y = species_number) +
  geom_point(colour = "pink", fill = "black", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="pink", se=FALSE)+
  labs(x = "Altitude", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#Número de espécies e precipitação

ggplot(data = da) +
  aes(x = precipitation, y = species_number) +
  geom_point(colour = "blue", fill = "violet", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="blue", se=FALSE)+
  labs(x = "Precipitação", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#Número de espécies e temperatura

ggplot(data = da) +
  aes(x = temperature, y = species_number) +
  geom_point(colour = "yellow", fill = "black", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="yellow", se=FALSE)+
  labs(x = "Temperatura", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#Os plots indicam que a quantidade de animais tendem a aumentar conforme aumenta a temperatura e quantidade pluviométrica.

#33. Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. Ah, inverta seu gráfico para a posição horizontal... e exporte utilizando o formato tidyverse

ta <- table(da$state_abbreviation)
ta

ta_por <- ta %>% 
  as.data.frame
colnames(ta_por) <- c("state", "freq")
ta_por

ggplot(data = ta_por) + 
  aes(x = state, y = freq) +
  geom_bar(color= ("black"), fill = ("#c51b8a"), stat = "identity") +
  coord_flip(horizontal=TRUE) +
  labs(x = "Estados",
       y = "Frequência de amostragem") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#34. Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

install.packages("ggpubr")
library(ggpubr)

ggboxplot(data = da, 
          x = "state_abbreviation", 
          y = "species_number",
          fill = c("#4292C6", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#6BAED6","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
                   "#b15928","#08519C","#ffffb3","#DEEBF7","#9ECAE1","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")

#ggpubr

ggbarplot(data=da,
          x = "state_abbreviation",
          y = "species_number", 
          fill = "state_abbreviation", 
          color = "state_abbreviation",
          palette = c("#2c7fb8", "#7fcdbb","#edf8b1","#de2d26","#fc9272","#fee0d2","#1c9099","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
                      "#a6bddb","#1c9099","#756bb1","#c51b8a","#fb8072","#80b1d3","#fa9fb5","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")+coord_flip()
#35. Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

da_drop_na <- da %>% 
  drop_na("coordinate_precision")
da_drop_na$coordinate_precision

ggboxplot(data = da_drop_na, #gerando o gráfico
          x = "coordinate_precision", 
          y = "species_number",
          fill = "coordinate_precision",
          palette = c("#636363, #f03b20, #feb24c, #e6550d"),
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none")

ggbarplot(data=da_drop_na,
          x = "coordinate_precision",
          y = "species_number", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          palette = c("#31a354, #f7fcb9, #addd8e, #1a354"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")

#37.Importe o arquivo vetorial de uso e cobertura da terra que você rodou a topologia (correção dos erros de topologia). No meu caso, salvei com esse nome: PE_2611606_USO_topologia.shp

library(tidyverse)
library (sf)
library(raster)
library(fasterize)
install.packages("fasterize")

setwd("C:/Users/kamil/Google Drive/disciplina-geoprocessamento/03_dados/01_vetor/recife_raw/recife_processamento")
dir()
rec_uso <- sf::st_read("PE_2611606_USO_topologia.shp")

rec_uso

plot(rec_uso[1] , main = NA, key.pos = NULL)



#38. Selecione e exporte apenas a classe formação florestal da coluna CLASSE_USO, criando o arquivo PE_2611606_USO_topologia_floresta.shp

rec_uso_floresta <- rec_uso %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")

rec_uso_floresta

plot(rec_uso_floresta[1], main=NA) 


setwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/07_exercicios/vetor r")

sf::st_write(rec_uso_floresta,"PE_2611606_USO_topologia_floresta.shp") 

#39. Importe esse novo arquivo vetorial ao projeto do QGIS, não se esqueça de ir salvando o projeto, vai que dá ruim...
#40. Use a ferramenta Vetor > Geometrias > Multipartes para partes simples para fazer com que cada polígono de floresta seja atribuído à uma linha diferente da tabela de atributos. Salve com o nome PE_2611606_USO_topologia_floresta_unico.shp

rec_uso_flor_un <- sf::st_cast(rec_uso_floresta,"POLYGON") # == Multipartes para partes simples no QGIS 
rec_uso_flor_un

plot(rec_uso_flor_un[5], main=NA, key.pos = NULL) #plotando

getwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/07_exercicios/vetor r")

sf::st_write(rec_uso_flor_un,"PE_2611606_USO_topologia_floresta_unico.shp") #exportando

#41. Crie uma coluna (interger) na tabela de atributos com o nome id, e utilizando a expressão $id, adicione um número diferente para cada polígono

rec_uso_flor_un_id <- rec_uso_flor_un %>% 
  dplyr::mutate(id=rec_uso_flor_un %>% nrow %>% seq()) #criando a coluna id e pedindo pra preencher as linhas com uma sequencia

rec_uso_flor_un_id

#42. Apague as outras colunas, deixando apenas a coluna id

uso_id <- rec_uso_flor_un_id %>% #criando um novo obj apenas com a coluna id
  dplyr::select(id)

uso_id

#43. Calcule a área (formato de coluna decimal) de cada fragmento em hectares na coluna a_frag_ha. Atente para usar $area/10000 para que o resultado seja em hectares.
#Você acaba de calcular uma das métricas mais comuns em Ecologia da Paisagem. Pode ser referida como "Conectividade estrutural"

uso_id_a <- uso_id %>%
  dplyr::mutate(a_frag_ha= sf::st_area(geometry/10000))
uso_id_a

#44. Faça um buffer com o valor negativo de 60 m, com o nome de PE_2611606_USO_topologia_floresta_unico_core60m.shp.
#Nesse momento vc acaba de gerar o vetor "core" com 60 metros de floresta de Recife. Calcule a área na coluna a_core_ha. Você acaba de calcular a área core dos fragmentos

uso_buf <- uso_id %>% 
  sf::st_buffer(-60)
uso_buf# Nesse momento vc acaba de gerar o vetor "core" com 60 metros de floresta de Recife. Calcule a área na coluna a_core_ha. Você acaba de calcular a área core dos fragmentos


uso_id_a_core <- uso_buf %>%
  dplyr::mutate(a_core_ha=st_area(geometry)/10000) #calculando a area core dos fragmentos
uso_id_a_core

plot(uso_id_a_core[3], main=NA, key.pos = NULL) #plotando

getwd()

sf::st_write(uso_buf,"PE_2611606_USO_topologia_floresta_unico_core60m.shp") #exportando

#45. diferença simetrica entre PE_2611606_USO_topologia_floresta_unico.shp e o vetor core: PE_2611606_USO_topologia_floresta_unico_core60m.shp

uso_edge <- sf::st_difference(rec_uso_flor_un, sf::st_combine(uso_id_a_core))

uso_edge
plot(uso_edge[5], main = NA, key.pos =NULL) #plotando

#46. Calcule a área com o nome da coluna sendo a_ed60m_ha

uso_a_edge <- uso_edge%>%
  dplyr::mutate(a_ed60m_ha=st_area(geometry/1000)) 
uso_a_edge
setwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/03_dados/raster")
dir()
sf::st_write(uso_a_edge,"PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

#47. Refaça os passos anteriores do QGIS (exercídios 37 a 46) 

setwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/03_dados/raster")
dir()
dem <- raster::raster("RECIFE_E_O_MUNDO_2.tif")
dem

# raster floresta unico
uso_id_a_raster <- fasterize::fasterize(sf = uso_id_a,
                                        raster = dem,
                                        field = "a_frag_ha")

uso_id_a_raster
plot(uso_id_a_raster)

# raster core60m

uso_id_core_raster <- fasterize::fasterize(sf = uso_id_a_core %>%
                                             dplyr::filter(!st_is_empty(.)) %>% 
                                             sf::st_cast("MULTIPOLYGON"),
                                           raster = dem,
                                           field = "a_core_ha")
uso_id_core_raster
plot(uso_id_core_raster)

#raster edge60m

uso_a_edge_raster <- fasterize::fasterize(sf = uso_a_edge %>%
                                            dplyr::filter(!st_is_empty(.)) %>% 
                                            sf::st_cast("MULTIPOLYGON"),
                                          raster = dem,
                                          field = "a_ed60m_ha")
uso_a_edge_raster
plot(uso_a_edge_raster)

# exportando os rasters

getwd("C:/Users/kamil/Dropbox/github/disciplina-geoprocessamento/07_exercicios/raster")

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
#48.Importe os arquivos raster gerados (com os valores de área, área core e área de borda) junto com o arquivo de DEM usado anteriormente num stack raster::stack()

st <- raster::stack(dem, uso_id_a_raster, uso_id_core_raster, uso_a_edge_raster)
st

names(st) <- c("dem", "id", "core", "edge")
plot(st)

#49. #49. Extraia os valores desses quatro rasters, compondo os mesmos num tibble
# Por fim, relacione esses valores das colunas (área, área core e área de borda - use o log10 para essas colunas, pois as unidades são diferentes) com o DEM
# Faça gráficos de dispersão (scatterplot)
# Há algum padrão? Discuta brevemente

#valores
st_va <- st %>% 
  raster::values() %>% 
  tibble::as_tibble()
st_va

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


ggsave("area_de_florestaxelevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando


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

ggsave("area_de_florestacorexelevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando
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

ggsave("area_de_florestabordaxelevacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando


# Quanto mais elevado o território, mais longe do centro urbano de recife, onde estão as áreas antropizadas. Portanto, os fragmentos florestais estão mais concentrados na periferia.



# FIM ---------------------------------------------------------------------


