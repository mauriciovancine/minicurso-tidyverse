# EXERCICIOS DISCIPLINAS DE GEOPROCESSAMENTO ------------------------------
#Luane Azeredo
# Wed Nov 20 22:12:57 2019 


#1. limpar o ambiente
rm (list = ls())

# calculos simples --------------------------------------------------------

#2.alguns calculos simples
(2*5) - (3^2)

#3.mais alguns calculos simples
log10(10) + log(100) * log2(1000)

# 4. Calcule o fatorial de 10: 10!
# Atribua ao objeto "fa_10"

fa_10 <- factorial(10)
fa_10

# 5. Em seguida, tire a raiz quadrada desse objeto, atribuindo à outro objeto "fa_10_rq"
fa_10_rq <- sqrt(3628800)
fa_10_rq

# mais cálculos simples

#6. Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas

vm <- (400/3.5) 
vm

# sequencias --------------------------------------------------------------
#7. crie uma sequencia de 0 a 10
seq_10 <- seq(from= 0, to= 10)
seq_10
# Some os elementos desse objeto cumulativamente, atribuindo ao objeto "seq_10_sum"

seq_10_sum <- sum(seq_10)
seq_10_sum

# 8. Crie uma sequência de 0 à 50, espaçada de 5 em 5
seq_50 <- seq(from= 0, to= 50, by= 5)
seq_50 
#9. Repita os elementos desse objeto 10 vezes sequencialmente, atribuindo ao objeto "seq_50_rep_times"
seq_50_rep_times <- rep(seq_50,10)
seq_50_rep_times


# lista -------------------------------------------------------------------
# amostras aleatorias -----------------------------------------------------

#10. Escolha 6 números para jogar na Mega-Sena durante um mês (duas vezes por semana)
# Atribua esses resultados à uma lista, de modo que cada elemento contenha os 6 números
# Lembrando: valores da Mega-Sena vão de 1 a 60

sena <- sample(1:60,6) #gerando seis valores aleatorios de seis num entre 1 e 60
sena

#reamostrando aleatoriamente para gerar 8 jogos diferentes

jogo1 <- sample(sena)
jogo2 <- sample(sena)
jogo3 <- sample(sena)
jogo4 <- sample(sena)
jogo5 <- sample(sena)
jogo6 <- sample(sena)
jogo7 <- sample(sena)
jogo8 <- sample(sena)


#criando uma lista de vetores (lista com os oito jogos)
li <- list (jogo1,jogo2,jogo3,jogo4,jogo5,jogo6,jogo7,jogo8)
li


# 11. Simule o resultado de 25 jogadas de um dado de 12 lados (sim, no RPG tem esse dado)
#replace=TRUE para quando as jogadas são maiores que o tamanho da amostra.

dado <- sample(1:12,25,replace=TRUE) #criando o dado (1:12) e pedindo para ele mostrar o resultado de 25 jogadas ,25. 

dado #resultado das jogadas do dado


# vetores -----------------------------------------------------------------

#12. Crie um vetor chamado "lo" para descrever 100 locais de amostragem. O vetor deve ter esse formato:local_1,...local_100

lo <- paste("local",1:100,sep="_")
lo

# 13. Crie um vetor chamado "lo" para descrever 100 locais de amostragem. Mas agora o vetor deve ter esse formato:Local_001,...local_100

lo <- c(paste0("local_00", 1:9), paste("local",10:99,sep="_0"), paste("local",100,sep="_"))
lo



# tidyverse ---------------------------------------------------------------

# 14. Crie um fator chamado "tr", com dois níveis ("cont" e "trat") para descrever 100 locais de amostragem, 50 de cada tratamento. 
# O fator deve ter esse formato:cont, cont, cont, ...., cont, trat, trat, trat, ...., trat

library(tidyverse)
tr <- rep(x = c("cont","trat"), each=50) %>%
  factor(c("cont", "trat"),
                levels = c("cont", "trat")) 
tr


# matriz ------------------------------------------------------------------

#15. Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ma <- c(sample(0:10,10000,replace=TRUE)) %>% #criando sequência aleatória
  matrix( nrow= 100, byrow= FALSE)#criando a matriz
ma


# pipes -------------------------------------------------------------------

#16. Reescreva essa operação utilizando pipes %>%: max(log(exp(sqrt(rpois(100, 5)))))
set.seed(1) 
max(log(exp(sqrt(rpois(100, 5))))) #sem pipes

set.seed(1)
rpois(100, 5) %>% #com pipes
  sqrt() %>% 
  exp() %>% 
  log() %>% 
  max()

# 17. Reescreva essa operação removendo os pipes %>%:


rnorm(100) %>% #com pipes
  exp() %>%
  log10() %>% 
  min() %>%
  sqrt()

#sem pipes

sqrt(min(log10(exp(rnorm(100)))))


# magrittr ----------------------------------------------------------------

# 18. Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by()


round(mean(sum(1:10)/3), digits = 1)#sem pipes

sum(1:10) %>%
  magrittr::divide_by(3)%>% #com pipes
  mean() %>% 
  round(digits = 1)
  

# importar tabela e manipulação dos dados---------------------------------------------------------
# 19
setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/03_dados/00_tabelas")
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

# 20 Utilize a função tibble::glimpse para verificar as colunas desses dados
tibble::glimpse(da)

# tidyr------------------
  
#21 Combine as colunas country, state, state_abbreviation, municipality, site, em uma coluna chamada local_total separadas por ,, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_uni <- da %>% 
  unite("local_total", country, state, state_abbreviation, municipality, site, sep=",") #atribuindo um novo objeto e unindo as colunas
da_uni$local_total #verificando a uniao das colunas



# 22 separar os valores da coluna passive_methods
da$passive_methods #verficando os valores da coluna passive methods

da_separate <- da %>% 
  separate("passive_methods", c("pt", "ar", "ft", "dr"), remove = FALSE)
da_separate[, c(1, 7:11)] #os valores ficarão dispostos da coluna 7 até a 11

# 23 Retire as linhas com NA da coluna year_start, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_drop_na <- da %>% 
  drop_na("year_start")
da_drop_na$year_start


# dplyr -------------------------------------------------------------------

# 24 Selecione todas as colunas que contenham method, atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_select <- da %>% 
  select(contains("method"))
da_select

#grafico

#25 Faça um histograma da coluna species_number utilizando o formato tidyverse

ggplot(data = da) +
  aes(species_number) +
  geom_histogram(fill= "#e34a33", color="#e9ecef")

setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/07_exercicios/graficos_R")
ggsave("histo_sp_number.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #salvando o grafico 


#26. Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação log10 das colunas altitude, temperature e precipitation e atribua ao mesmo objeto da utilizando o formato tidyverse

da_mutate <- da %>% 
  mutate(alt_log= log(altitude), tem_log= log(temperature), pre_log= log(precipitation))
da_mutate

#27. Ordene os dados em forma decrescente pela coluna altitude, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_arrange <- da %>% 
  arrange(altitude)
da_arrange

# 28. Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_filter <- da %>% 
  filter(altitude > 5 & temperature < 15 | precipitation  >1000 & precipitation <= 1500 ) 
da_filter

# 29. Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_sample_n <- da %>% 
  filter(species_number>15) %>% 
  sample_n(200)
da_sample_n

# 30. Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr utilizando o formato tidyverse

da_range <- da %>%
  select(species_number, altitude, temperature, precipitation) %>% 
  map(range, na.rm=TRUE)
da_range

# 31. Cometi um grave erro no data paper de anfíbios... Algo relacionado à colunas com dados separados com vírgulas, aí quando se abre num .csv (separado por vírgulas) dá ruim... Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos

#RESPOSTA: para o caso de dados separados com vírgulas, o recomendado é utilizar a função read_csv2 para ler os dados e write_csv2 para que os dados de cada coluna sejam separados por ";" na hora de salvar
#Caso a tabela seja salvo no formato de separação por vírgulas, funções do formato tydr como 'separate' ou 'unite' podem não ser capazes de diferencir valores que estão em diferentes colunas de valores decimais separados por virgulas.
setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/07_exercicios")
da_save <- write_csv2(da, "ATLANTIC_AMPHIBIANS_sites.csv", na = "NA", append = FALSE, col_names = TRUE,
           quote_escape = "double") #salvando o datapaper de anfíbios com separação de colunas por ";"

#32. Gere gráficos no ggplot2 relacionando o número de espécies e as variáveis altitude, temperature, precipitation, exportando cada um deles, utilizando o formato tidyverse.
# Responda: existe alguma relação?

#escolhendo o diretorio para salvar os gráficos
setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/07_exercicios/graficos_R")


library(ggplot2)

ggplot(data = da) +
  aes(x = altitude, y = species_number) +
  geom_point(colour = "black", fill = "blue", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Altitude", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

#a função ggsave salva o grafico no disco
ggsave("spp_altitude.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #salvando o grafico ssp x altitude

#grafico de precipitação

ggplot(data = da) +
  aes(x = precipitation, y = species_number) +
  geom_point(colour = "black", fill = "orange", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Precipitação", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

#salvando
  ggsave("spp_precipitacao.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

#grafico de temperatura
  
ggplot(data = da) +
  aes(x = temperature, y = species_number) +
  geom_point(colour = "black", fill = "green", size = 2, 
             alpha = .5, pch = 21) +
  geom_smooth(method=lm , color="black", se=FALSE)+
  labs(x = "Temperatura", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#salvando
ggsave("spp_temperatura.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

# os graficos indicam que o numeros de especies de anfíbios aumenta conforme precipitação e altitude aumentam 
# além disso, o número de spp tende  a aumentar à medida que a temperatura diminui. Esses resultados são esperados
# tendo em vista que anfíbios são animais de habitat semi-aquático, vivendo porntanto em locais úmidos e de temperatura não elevada.

# 33. Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. 
# Ah, inverta seu gráfico para a posição horizontal... e exporte utilizando o formato tidyverse

ta <- table(da$state_abbreviation) #selecionando a frequencia por estado
ta

# frequency table as data frame
ta_por <- ta %>% #organizando a frequencia em uma nova coluna, criando uma tabela como data.frame
  as.data.frame
colnames(ta_por) <- c("state", "freq")
ta_por

#  #gerando o gráfico da frequencia por estado 
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

# exportando o gráfico

ggsave("state_freq.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE)

# 34. Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte
library(ggpubr)

#boxplot
ggboxplot(data = da, 
          x = "state_abbreviation", 
          y = "species_number",
          fill = c("#a6cee3", "#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99",
                   "#b15928","#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5"),
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")

ggsave("ne_state.tiff", plot = last_plot(), device = tiff, width = 1000, height = 600, units = "in",
       dpi = 300, limitsize = FALSE)

#barplot

ggbarplot(data=da,
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


# 35. Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

da_drop_na <- da %>% #retirando os valores vazios
  drop_na("coordinate_precision")
da_drop_na$coordinate_precision

# boxplot
ggboxplot(data = da_drop_na, #gerando o gráfico
          x = "coordinate_precision", 
          y = "species_number",
          fill = "coordinate_precision",
          palette = c("#1b9e77, #7570b3, #e7298a, #66a61e"),
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none")

#exportando
ggsave("ne_gps.tiff", plot = last_plot(), device = tiff, width = 500, height = 500, units = "in",
       dpi = 300, limitsize = FALSE) #exportando

# barplot

ggbarplot(data=da_drop_na,
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

# vetor -------------------------------------------------------------------

#Dicas: sf::st_cast("POLYGON") == Multipartes para partes simples no QGIS 
# sf::st_difference(, sf::st_combine()) == Diferença simétrica no QGIS

# 37. Importe o arquivo vetorial de uso e cobertura da terra que você rodou a topologia (correção dos erros de topologia). No meu caso, salvei com esse nome: PE_2611606_USO_topologia.shp
library(tidyverse)
library (sf)
library(raster)
library(fasterize)
setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/03_dados/01_vetor/recife_raw/recife_processamento")

# importando arquivo vetorial

rec_uso <- sf::st_read("PE_2611606_USO_topologia.shp")

rec_uso

plot(rec_uso[5] , main = NA, key.pos = NULL)



# 38. Selecione e exporte apenas a classe formação florestal da coluna CLASSE_USO, criando o arquivo PE_2611606_USO_topologia_floresta.shp

rec_uso_floresta <- rec_uso %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")

rec_uso_floresta

plot(rec_uso_floresta[5], main=NA) #plotando o arquivo formação florestal

setwd ("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/07_exercicios/vetor_r")

sf::st_write(rec_uso_floresta,"PE_2611606_USO_topologia_floresta.shp") #exportando vetor

# 39. Importe esse novo arquivo vetorial 
# fazer com que cada polígono de floresta seja atribuído à uma linha diferente da tabela de atributos. Salve com o nome PE_2611606_USO_topologia_floresta_unico.shp

rec_uso_flor_un <- sf::st_cast(rec_uso_floresta,"POLYGON") # == Multipartes para partes simples no QGIS 
rec_uso_flor_un

plot(rec_uso_flor_un[5], main=NA, key.pos = NULL) #plotando
getwd()

sf::st_write(rec_uso_flor_un,"PE_2611606_USO_topologia_floresta_unico.shp") #exportando

# 41. Crie uma coluna (interger) na tabela de atributos com o nome id, e utilizando a expressão $id, adicione um número diferente para cada polígono

rec_uso_flor_un_id <- rec_uso_flor_un %>% 
  dplyr::mutate(id=rec_uso_flor_un %>% nrow %>% seq()) #criando a coluna id e pedindo pra preencher as linhas com uma sequencia

rec_uso_flor_un_id

# 42. Apague as outras colunas, deixando apenas a coluna id

uso_id <- rec_uso_flor_un_id %>% #criando um novo obj apenas com a coluna id
  dplyr::select(id)
  
uso_id
  
# 43. Calcule a área (formato de coluna decimal) de cada fragmento em hectares na coluna a_frag_ha. Atente para usar $area/10000 para que o resultado seja em hectares.
# Você acaba de calcular uma das métricas mais comuns em Ecologia da Paisagem. Pode ser referida como "Conectividade estrutural"

uso_id_a <- uso_id %>%
  dplyr::mutate(a_frag_ha= sf::st_area(geometry/10000))
uso_id_a

  
# 44. Faça um buffer com o valor negativo de 60 m, com o nome de PE_2611606_USO_topologia_floresta_unico_core60m.shp.

uso_buf <- uso_id %>% 
  sf::st_buffer(-60)
uso_buf# Nesse momento vc acaba de gerar o vetor "core" com 60 metros de floresta de Recife. Calcule a área na coluna a_core_ha. Você acaba de calcular a área core dos fragmentos


uso_id_a_core <- uso_buf %>%
  dplyr::mutate(a_core_ha=st_area(geometry)/10000) #calculando a area core dos fragmentos
uso_id_a_core

plot(uso_id_a_core[3], main=NA, key.pos = NULL) #plotando

getwd()

sf::st_write(uso_buf,"PE_2611606_USO_topologia_floresta_unico_core60m.shp") #exportando


#45 diferença simetrica entre PE_2611606_USO_topologia_floresta_unico.shp e o vetor core: PE_2611606_USO_topologia_floresta_unico_core60m.shp

uso_edge <- sf::st_difference(rec_uso_flor_un, sf::st_combine(uso_id_a_core))

uso_edge
plot(uso_edge[5], main = NA, key.pos =NULL) #plotando


# Calcule a área com o nome da coluna sendo a_ed60m_ha

uso_a_edge <- uso_edge%>%
  dplyr::mutate(a_ed60m_ha=st_area(geometry/1000)) 
uso_a_edge # Você acaba de calcular a área de borda desses fragmentos

getwd()
sf::st_write(uso_a_edge,"PE_2611606_USO_topologia_floresta_unico_edge60m.shp")


# Raster ------------------------------------------------------------------

# 46. Converta esses três vetores para raster: 
# PE_2611606_USO_topologia_floresta_unico.shp, 
# PE_2611606_USO_topologia_floresta_unico_core60m.shp e 
# PE_2611606_USO_topologia_floresta_unico_edge60m.shp


# importar raster recife_e_o_mundo2

setwd("C:/Users/luaaz_000/Desktop/Geoprocessamento/disciplina-geoprocessamento_Luane/07_exercicios/raster_r")
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
getwd()
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

#49. Extraia os valores desses quatro rasters, compondo os mesmos num tibble
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


# todos os graficos apresentam uma correlação positiva com a elevação. Deste modo, podemos discutir que quanto mais
# elevado, mais longe do centro urbano de recife, onde estão as áreas antropizadas. Portanto, os fragmentos florestais estão mais concentrados na periferia.



# FIM ---------------------------------------------------------------------


