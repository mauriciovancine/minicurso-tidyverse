##-------------------------------------------------------------------------

#Arthur Ramalho Magalhães
# Sat Oct 26 14:19:09 2019 ------------------------------
#Objetivo:????
setwd("/home/mude/data/github/disciplina-geoprocessamento/07_exercicios/respostas_exercicios/arthur")
setwd("C:\\Users\\bsmah\\OneDrive\\?rea de Trabalho\\backup arthur\\geoprocessamento\\07_exercicios")
#limprando enviroment
rm(list = ls())
#calculo simlples
(2*5)-(3^2)

#mais calculos simples
# r não entende 'ln' pesquisei e logaritmo narual é simplesmente log
a<-log10(10) +log(100) * log2(1000)
a
# fatorial - calcula fatorial 10!
factorial(10)
#atribuindo ao objeto
fa_10<-factorial(10)
fa_10
fa_10_rq<-sqrt(fa_10)
fa_10_rq

#Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas
s<-400
t<-3.5
Vm<-s/t
Vm
# criando sequencia de 0:10 e depois somando os valores 
seq_10<-(0:10)
seq_10_sum<- sum(seq_10)
# Criando sequencia de 0 a 50 espaçada de 5 em 5
seq_50<-seq(0, 50, by=5)
#agora repita os valores sequecialmente 10 vezes
##exemplo que achei na net: rep(seq(0,60,15), times = 3)
seq_50_rep_times<-rep(seq_50, times=3)
seq_50_rep_times


#10. Escolha 6 números para jogar na Mega-Sena
#durante um mês (duas vezes por semana):
megasena<-c(32, 44, 9, 54, 33, 21)

#Atribua esses resultados à uma lista, 
#de modo que cada elemento contenha os 6 números
d<-rbind(megasena, megasena, megasena, megasena, megasena, megasena)
d2<-as.data.frame(d)

d3<-d2[sample(1:nrow(d2)), ]

mega<-as.list(as.data.frame(d))

#11. Amostragens aleatórias
#Simule o resultado de 25 jogadas de um dado de 12 lados
sample.int(12, 25, replace=TRUE)

#12.crie um vetor chamado "lo" para descrever
#100 locais de amostragem. o vetor deve ter esse formato
#:local_1, local_2, local_3, ...., local_100
numeros<-c(1:100)
nomes<-"local"
nomes<-rep.int(nomes, 100)
locais<-paste(nomes, numeros, sep = "_")
lo<-as.vector(locais)
#Mas agora o vetor deve ter esse formato:
#local_001, local_002, local_003, ...., local_100
number<-1:100
paste("0",number)
library(stringr)
number2<-str_pad(number, 3, pad = "0")
number3<-paste(nomes, number2, sep="_")
#14. Crie um fator chamado "tr", com dois níveis ("cont" e "trat")
#para descrever 100 locais de amostragem, 50 de cada tratamento.
#O fator deve ter esse formato:
#cont, cont, cont, ...., cont, trat, trat, trat, ...., trat
trat<-c("cont", "trat")
as.factor(sort(rep.int(trat, 100)))
#15. Crie uma matriz chamada "ma", da disposição de um vetor 
#composto por 10000 valores aleatórios entre 0 e 10. 
#A matriz deve conter 100 linhas e ser disposta por colunas
ma<-matrix(sample(0:10, 1000,replace=T), 100, ncol=10)

#16. Reescreva essa operação utilizando pipes %>%:
library(tidyverse)
max(log(exp(sqrt(rpois(100, 5)))))

rpois(100, 5) %>% sqrt() %>%  exp() %>% max() %>% log()

#17. Reescreva essa operação removendo os pipes %>%:
rnorm(100) %>% exp() %>% log10() %>% min() %>% sqrt() 

sqrt(min(log10(exp(rnorm(100)))))
#18. Reescreva essa operação utilizando pipes %>% e
#a função magrittr::divide_by()
round(mean(sum(1:10)/3), digits = 1)
library(magrittr)
round(mean(sum(1:10)/3), digits = 1)

sum(1:10) %>% 
  mean() %>% 
  magrittr::divide_by(3) %>% 
  round(digits=1) 
#19. Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse
setwd("/home/arthur/Área de Trabalho/geoprocessamento/07_exercicios")
da<-readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
da2<-readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")


dir()
head(da2)

#20. Utilize a função tibble::glimpse para verificar as colunas
#desses dados
tibble::glimpse(da)
tibble::glimpse(da2)

#21. Combine as colunas country, state, state_abbreviation,
#municipality, site, em uma coluna chamada local_total separadas por ,, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse
library(tidyr)
tibble::glimpse(da3)
head(da3)
da3<-unite(da2,
      local_total,
      c(country, state, state_abbreviation,municipality, site),
      sep=",")
str(da3)
#22. Separe a coluna passive_methods em outras colunas (mesmo com o erro...),
#atribuindo o resultado a um novo objeto,
#utilizando o formato tidyverse
da3$passive_methods
da4<-da3 %>% separate(passive_methods, c("pmchar1","pmchar2"))
head(da4)
?separate
#23. Retire as linhas com NA da coluna year_start, 
#atribuindo o resultado a um novo objeto,
#utilizando o formato tidyverse
da5<-da4 %>% drop_na(year_start)
str(da5)
#24. Selecione todas as colunas que contenham method, 
#atribuindo o resultado a um novo objeto, utilizando o formato tidyverse
library(dplyr)
da6<-da5 %>% select(da5, ends_with("_method"))
head(da6)
str(da6)

#25. Faça um histograma da coluna species_number utilizando
# o formato tidyverse
hist(da5$species_number)

library(ggplot2)
ggplot(da5, aes(species_number)) + 
  geom_histogram()
#26.Adicione essas novas colunas alt_log, 
#tem_log e pre_log, que são a operação log10 das
#colunas altitude, temperature e precipitation e atribua ao mesmo
#objeto da utilizando o formato tidyverse.
da5 %>%  mutate(tem_log = log10(temperature))
da5 %>%  mutate(alt_log = log10(altitude))
da5 %>%  mutate(pre_log = log10(precipitation))

#27. Ordene os dados em forma decrescente pela coluna altitude,
#atribuindo o resultado a um novo objeto utilizando o formato
#tidyverse
#starwars %>% arrange(desc(mass))
da7<-da5 %>% arrange(desc(altitude))

#28Filtre as linhas com altitude maior que 1000 mm,
#temperature menor que 15 ºC ou precipitation maior que 
#1000 mm e menor ou igual que 1500 mm, atribuindo o resultado
#a um novo objeto utilizando o formato tidyverse
da8<-da5 %>% filter(altitude>1000 & temperature < 15 & precipitation >1000 & precipitation <=1500)
head(da8)
#29. Amostre 200 linhas aleatoriamente com número de espécies 
#maior que 15 espécies, atribuindo o resultado a um novo objeto
#utilizando o formato tidyverse.
da5
da9<-da5 %>% filter(species_number>15) %>%  sample_n(200)
da9

#30. Calcule o range sem os NAs, para as colunas species_number, 
#altitude, temperature, precipitation usando o pacote purrr
#utilizando o formato tidyverse
head(da5)
da
library(tidyverse)
da5

da5 %>% range(da5$species_number, na.rm=TRUE)
da5 %>% range(da5$altitude, na.rm=TRUE)
da5 %>% range(da5$temperature,  na.rm=TRUE)
da5 %>% range(da5$temperature,  na.rm=TRUE)

#31 Cometi um grave erro no data paper de anf?bios... 
#Algo relacionado ? colunas com dados separados com 
#v?rgulas, a? quando se abre num .csv (separado por 
#v?rgulas) d? ruim... Ache uma solu??o atrav?s das fun??es
#do tidyverse e explique nos termos do formato tidyr 
#porque esses dados est?o err?neos
library(readr)
sites<-read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")
#32. Gere gr?ficos no ggplot2 relacionando o n?mero de
#esp?cies e as vari?veis altitude, temperature,
#precipitation, exportando cada um deles, utilizando
#o formato tidyverse. Responda: existe alguma rela??o
library(ggplot2)
setwd("C:\\Users\\bsmah\\OneDrive\\?rea de Trabalho\\backup arthur\\geoprocessamento\\07_exercicios")
#altitude x n de esp?cies
ggplot(data = da5) +
  aes(x = altitude, y = species_number) +
  labs(x = "Altitude", y = "Species number") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("spp_number_altitude.tiff", wi = 20, he = 15, un = "cm", dpi = 300)
#temperatura x n de esp?cies
ggplot(data = da5) +
  aes(x = temperature, y = species_number) +
  labs(x = "Temperature", y = "Species number") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("temp_sp_number_amph.tiff", wi=20, he=15, un="cm", dpi=300)
#precipitation x n de especiess
ggplot(data = da5) +
  aes(x = precipitation, y = species_number) +
  labs(x = "Precipitation", y = "Species Number") +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
ggsave("prec_sp_number_amph.tiff", wi=20, he=15, un="cm", dpi=300)
#resposta#####
#Existem rela??es positivas entre precipita??o e altitude
#com o n?mero de esp?cies e um rela??o negativa fraca entre temperatura e n?mero
#de esp?cies. todas rela??e aparentam ser fracas pela 
#inclina??o da reta, onde a mais expressiva delas seria a precipita??o.


#33 Gere gr?ficos no ggplot2 mostrando a frequ?ncia 
#absoluta de amostragem em cada estado e exporte. Ah,
#inverta seu gr?fico para a posi??o horizontal... e 
#exporte utilizando o formato tidyverse 
da5
ta <- table(da2$state_abbreviation) #--- selecionar a frequência por estado
ta
ta_por <- ta %>% #--- frequência como data.frame
  as.data.frame()
colnames(ta_por) <- c("state", "freq")
ta_por
ggplot(data = ta_por) +
  aes(x = state, y = freq) +
  geom_bar(color = ("black"), fill = ("#c51b8a"), stat = "identity") +
  labs(x = "Estados",
       y = "Frequência de amostragem") +
  coord_flip() #--- inverter a posição do gráfico
ggsave("freq_state_samplesize.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#34. Gere gr?ficos no ggpubr relacionando o n?mero de esp?cies e os estados e exporte
library(ggpubr)
ggbarplot(data=da2,
          x = "state_abbreviation",
          y = "species_number", 
          palette = c("Harmonic"),
          lab.size = 5,
          xlab = "Estados",
          ylab = "Numero de especies")
ggsave("number_sp_state.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#35. Gere gr?ficos no ggpubr relacionando o n?mero de esp?cies e a precis?o do GPS e exporte
ggbarplot(data=da2,
          x = "coordinate_precision",
          y = "species_number", 
          fill = "coordinate_precision", 
          color = "coordinate_precision",
          label = FALSE, 
          lab.size = 5,
          xlab = "GPS precision",
          ylab = "Number of species")
ggsave("gps_precision_spp.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

#36 Crie uma pasta dentro da pasta 07_exercicios, com o nome de exercicios_qgis.
#Crie um projeto novo chamado proj_qgis_exercicio_utm_25s_sirgas2000. Todos os 
#arquivos processados devem ser salvos nessa pasta. No final, envie essa pasta
#junto com o projeto do QGIS.
setwd("C:\\Users\\bsmah\\OneDrive\\?rea de Trabalho\\backup arthur\\geoprocessamento\\07_exercicios")
dir.create("exercicios_qgis.")

#37. Importe o arquivo vetorial de uso e cobertura da terra que voc? rodou a topologia (corre??o dos erros de topologia). No meu caso, salvei com esse nome: PE_2611606_USO_topologia.shp
feito
#38. Selecione e exporte apenas a classe forma??o florestal da coluna CLASSE_USO, criando o arquivo PE_2611606_USO_topologia_floresta.shp
feito
#39.Importe esse novo arquivo vetorial ao projeto do QGIS, n?o se esque?a de ir salvando o projeto, vai que d? ruim...
feito
#40 Use a ferramenta Vetor > Geometrias > Multipartes para partes simples para fazer com que cada pol?gono de floresta seja atribu?do ? uma linha diferente da tabela de atributos. Salve com o nome PE_2611606_USO_topologia_floresta_unico.shp
feito
#41. Crie uma coluna (interger) na tabela de atributos com o nome id, e utilizando a express?o $id, adicione um n?mero diferente para cada pol?gono
feito
#42. Apague as outras colunas, deixando apenas a coluna id
feito
#43. Calcule a ?rea (formato de coluna decimal) de cada fragmento em hectares na coluna a_frag_ha. Atente para usar $area/10000 para que o resultado seja em hectares.
feito
#44 . Fa?a um buffer com o valor negativo de 60 m, com o nome de PE_2611606_USO_topologia_floresta_unico_core60m.
feito
#45 Use a ferramenta Vetor > Geoprocessamento > Diferen?a sim?trica
#A primeira entrada ser? o vetor de floresta: PE_2611606_USO_topologia_floresta_unico.shp
#A segunda entrada ser? o vetor core: PE_2611606_USO_topologia_floresta_unico_core60m.shp
#Salve como: PE_2611606_USO_topologia_floresta_unico_edge60m.shp
#Calcule a ?rea com o nome da coluna sendo a_ed60m_ha
#Voc? acaba de calcular a ?rea de borda desses fragmentos
feito
#46 Converta esses tr?s vetores para raster: PE_2611606_USO_topologia_floresta_unico.shp, PE_2611606_USO_topologia_floresta_unico_core60m.shp e PE_2611606_USO_topologia_floresta_unico_edge60m.shp 
#Utilize a ferramenta Raster > Converter > Converter Vetor para Raster (rasterizar) e as colunas de ?rea respectivas de cada arquivo Utilize para a rasteriza??o, a resolu??o de 90 m, selecionando o
#Output raster size units como Georeferenced units, e o arquivo dem_mosaico_recife_e_o_mundo2.tif como base para a extens?o de sa?da 
#Salve com os mesmos nomes dos shapefiles e no formato GeoTiff
feito
#-------------geoprocessamento no R-#####
#37
library(sf)
#install.packages("fasterize")
library(fasterize)
library(raster)
#install.packages("tmap")
library(tmap)
library(tidyverse)
getwd()
dir()
setwd("C:\\Users\\bsmah\\OneDrive\\?rea de Trabalho\\backup arthur\\geoprocessamento\\07_exercicios")
setwd("exercicios_qgis")
PE_uso_area <- sf::st_read("PE_2611606_USO_topologia.shp") 
plot(PE_uso_area[1]) 
#38

PE_uso_tab <-  sf::st_drop_geometry(PE_uso_area)
PE_uso_tab
PE_uso_tab$CLASSE_USO   
# editando coluna dentro da tabela de atributos da feicao
PE_uso_floresta <- PE_uso_area %>% 
  dplyr::filter(CLASSE_USO == "forma??o florestal")
PE_uso_floresta
plot(PE_uso_floresta[1])
sf::st_write(PE_uso_floresta, "PE_2611606_USO_topologia_floresta_R.shp")

#40
pe_uso_floresta_unico <- sf::st_cast(PE_uso_floresta, "POLYGON") #-- multiplas partes, para simples (ferramenta QGis)
pe_uso_floresta_unico
plot(pe_uso_floresta_unico[5]) #--- plotar
sf::st_write(pe_uso_floresta_unico, "PE_2611606_USO_topologia_floresta_unico_R.shp")

#41
pe_uso_flo_unico_id <- pe_uso_floresta_unico %>%
  dplyr::mutate(id = pe_uso_floresta_unico %>% nrow %>% seq) #--- criar uma coluna
pe_uso_flo_unico_id
#42
pe_uso_flo_unico_id <- pe_uso_flo_unico_id %>%
  dplyr::select(id) #--- selecionar uma coluna e deletar as outras
pe_uso_flo_unico_id
#43

pe_uso_flo_unico_id_area <- pe_uso_flo_unico_id %>%
  dplyr::mutate(a_frag_ha = sf::st_area(pe_uso_flo_unico_id)/10000) #--- area dos fragmentos em ha
pe_uso_flo_unico_id_area
#44
pe_uso_flo_unico_id_core <- pe_uso_flo_unico_id %>% 
  sf::st_buffer(-60) %>%
  dplyr::mutate(a_core_ha = sf::st_area(.)/10000)
pe_uso_flo_unico_id_core

tm_shape(pe_uso_flo_unico_id_core) +
  tm_polygons()


sf::st_write(pe_uso_flo_unico_id_core, "PE_2611606_USO_topologia_floresta_unico_core60m_R.shp")


#45
pe_uso_flo_unico_id_edge <- pe_uso_flo_unico_id %>% 
  sf::st_difference(sf::st_combine(pe_uso_flo_unico_id_core)) %>% 
  dplyr::mutate(a_edge_ha = sf::st_area(.)/10000)
pe_uso_flo_unico_id_edge

tm_shape(pe_uso_flo_unico_id_edge) +
  tm_polygons()
sf::st_write(pe_uso_flo_unico_id_core, "PE_2611606_USO_topologia_floresta_unico_edge60m_R.shp")

#46

dem <- raster::raster("C:\\Users\\bsmah\\OneDrive\\?rea de Trabalho\\backup arthur\\geoprocessamento\\07_exercicios\\exercicios_qgis\\dem_mosaico_recife_e_o_mundo2.tif")
dem
plot(dem)

pe_uso_flo_unico_id_area_raster <- fasterize::fasterize(sf = pe_uso_flo_unico_id_area,
                                                     raster = dem,
                                                     field = "a_frag_ha")
plot(pe_uso_flo_unico_id_area_raster)

pe_uso_flo_unico_id_core_raster <- fasterize::fasterize(sf = pe_uso_flo_unico_id_core %>%
                                                       dplyr::filter(!st_is_empty(.)) %>% 
                                                       sf::st_cast("MULTIPOLYGON"),
                                                     raster = dem,
                                                     field = "a_core_ha")
pe_uso_flo_unico_id_core_raster
plot(pe_uso_flo_unico_id_core_raster)

pe_uso_flo_unico_id_edge_raster <- fasterize::fasterize(sf = pe_uso_flo_unico_id_edge %>%
                                                       dplyr::filter(!st_is_empty(.)) %>% 
                                                       sf::st_cast("MULTIPOLYGON"),
                                                     raster = dem,
                                                     field = "a_edge_ha")
pe_uso_flo_unico_id_edge_raster
plot(pe_uso_flo_unico_id_edge_raster)

dir()
setwd("tifs")
library(raster)
raster::writeRaster(x = pe_uso_flo_unico_id_area_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_R",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")


raster::writeRaster(x = pe_uso_flo_unico_id_core_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60_R",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = pe_uso_flo_unico_id_edge_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m_R",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")
#48####
setwd("exercicios_qgis")
st <- raster::stack(dem, pe_uso_flo_unico_id_area_raster, pe_uso_flo_unico_id_core_raster, pe_uso_flo_unico_id_edge_raster)
st

names(st) <- c("dem", "flo", "core", "edge")
plot(st)
#48####
library(tibble)
library(raster)

st_va <- st %>% 

  raster::values() %>% 

  tibble::as_tibble() 
  
st_va

#49
#--- Rela??o entre as ?reas de floresta e a eleva??o
# Floresta (total)


st_va

ggplot(data = st_va) +
  aes(x = dem, y = log10(flo + 1)) +
  geom_point() +
  labs(x = "Eleva??o (m)", y = "?rea de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

# area core
ggplot(data = st_va) +
  aes(x = dem, y = log10(core + 1)) +
  geom_point() +
  labs(x = "Eleva??o (m)", y = "?rea de floresta core (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()

# area das bordas (edge)
ggplot(data = st_va) +
  aes(x = dem, y = log10(edge + 1)) +
  geom_point() +
  labs(x = "Eleva??o (m)", y = "?rea de borda de floresta (log10)") +
  stat_smooth(method = "lm") +
  theme_bw()
#aparentemente os fragmentos de floresta que se encontram em maior eleva??o possuem 
#mais ?rea total, de n?cleo e de borda, isso tem a ver com o padr?o de ocupa??o hist?rico
#da cidade que ocupou primeiramente as ?reas mais baixas, deixando os morros como ?reas geralmente
#n?o valorizadas. Al?m disso, recife possui uma forte hist?ria com as planta??es de cana e engenhos,
# que ocupavam boa parte da cidade e as ?reas de florestas mais elevadas eram poupadas
#j? que h? dificuldade de manter planta??es em terrenos inclinados.
