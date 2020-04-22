# Script contendo a resolução dos exercícios propostos para a disciplina de geoprocessamento
# Paulo Sérgio Monteiro Ferreira
#25/11/2019

# Pacotes

library(tidyverse)
library(purrr)
library(ggplot2)
library(radiant.data) # ln não está no base
library(GGally)
library(ggpubr)
library(dplyr)
library(raster)
library(rgdal)
library(sf)
library(fasterize)
library(tibble)
library(gridExtra)

# Diretório

setwd("C:/Users/sergi/Desktop/07_exercicios")

# Limpar ambiente 

rm(list = ls())

# 4 Vamos começar com alguns cálculos simples

(2 * 5) - (3 ^ 2)

# 5 Mais alguns cálculos simples

log10(10) + ln(100) * log2(1000)

# 6 Ainda mais alguns cálculos simples

fa_10<-factorial(10)# Fatorial
fa_10

fa_10_rq<-sqrt(fa_10)# Raiz quadrada
fa_10_rq

# 7 Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas

400/3.5

# 8 

seq_10<-0:10 # Crie uma sequência unitária de 0 à 10
seq_10

seq_10_sum<-sum(seq_10) # Some os elementos desse objeto cumulativamente
seq_10_sum

# 9

seq_50 <- seq(from = 0, to = 50, by = 5) # Crie uma sequência de 0 à 50, espaçada de 5 em 5
seq_50

seq_50_rep_times<-rep(seq_50, times = 10) # Repita os elementos desse objeto 10 vezes sequencialmente
seq_50_rep_times

# 10 Escolha 6 números para jogar na Mega-Sena durante um mês (duas vezes por semana)

mega_mes <- list(NULL)
for (i in 1:8) {
  mega_mes <- list(sample(1:60, 6))
  print(mega_mes)
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
tr

#15 Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ve<-sample(0:10,10000,replace = T)

ma <- matrix(data = ve, nrow = 100, ncol = 100, byrow = FALSE)
ma

# 16 Reescreva essa operação utilizando pipes %>%

max(log(exp(sqrt(rpois(100, 5)))))

result <- rpois(100, 5) %>% 
  sqrt() %>%
  exp() %>% 
  log() %>% 
  max()
result  

# 17 Reescreva essa operação removendo os pipes %>%

rnorm(100) %>%
  exp() %>%
  log10() %>% 
  min() %>%
  sqrt()

result<- sqrt(min(log10(exp(rnorm(100)))))

# 18 Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by()

round(mean(sum(1:10)/3), digits = 1)

sum(1:10)%>%
  magrittr::divide_by(3) %>% 
  mean()%>%
  round(1)
  

# 19 Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse

# import sites
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

# 20 Utilize a função tibble::glimpse para verificar as colunas desses dados

glimpse(da)

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

da <- da %>% 
  mutate(alt_log = log10(altitude),tem_log = log10(temperature),pre_log = log10(precipitation))
da

# 27 Ordene os dados em forma decrescente pela coluna altitude, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_ord <- da %>% 
  arrange(desc(altitude))
da_ord

# 28 Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_filter <- da %>% 
  filter(altitude > 1000, temperature < 15 |precipitation >1000 | precipitation <=1500) 
da_filter

# 29 Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_sam <- da %>% filter(species_number > 15) %>% sample_n(200)
da_sam

# 30 Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr utilizando o formato tidyverse

range(da$species_number, na.rm=TRUE)
range(da$altitude, na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)
range(da$temperature,  na.rm=TRUE)

# 31 Cometi um grave erro no data paper de anfíbios... Algo relacionado à colunas com dados separados com vírgulas, aí quando se abre num .csv (separado por vírgulas) dá ruim... Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errôneos

# Resposta: A forma correta de ler um dado csv separado por vírgulas no Tidyverse é através da função read_csv, o que resolveria o erro de leitura. Caso fosse ";" como separador, o correto seria usar read_csv2.



# 32 Gere gráficos no ggplot2 relacionando o número de espécies e as variáveis altitude, temperature, precipitation, exportando cada um deles, utilizando o formato tidyverse. Responda: existe alguma relação?

da_alt <- da %>% 
  select(species_number, altitude)

da_temp <- da %>% 
  select(species_number, temperature)

da_pres <- da %>% 
  select(species_number, precipitation)

# altitude
ggpairs(data = da_alt) +
  theme_bw()# Correlação positiva entre número de espécies e altitude

ggsave("da_alt.svg", height = 14, width = 18, dpi=900, units = "cm")

# temperatura
ggpairs(data = da_temp) +
  theme_bw()# Correlação negativa entre número de espécies e temperatura

ggsave("da_temp.svg", height = 14, width = 18, dpi=900, units = "cm")

# precipitação
ggpairs(data = da_pres) +
  theme_bw()# Correlação negativa entre número de espécies e precipitação

ggsave("da_pres.svg", height = 14, width = 18, dpi=900, units = "cm")

# 33 Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. Ah, inverta seu gráfico para a posição horizontal... e exporte utilizando o formato tidyverse


da$state2<-iconv(da$state, to='ASCII//TRANSLIT')# Remoção dos acentos problemáticos nos nomes dos estados

da_fa <-da %>% select("state2", "effort_months") %>% na.omit() %>% group_by(state2) %>% summarise(freq_tt = sum(effort_months))

g4 <- ggplot(da_fa, aes(x = state2, y = freq_tt)) + geom_bar(stat="identity")+ scale_fill_grey()+
  coord_flip()

g4

ggsave("da_fa.svg", height = 14, width = 18, dpi=900, units = "cm")

# 34 Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

da_fa2 <-da %>% 
  dplyr::select("state_abbreviation", "species_number") %>%      na.omit() %>% group_by(state_abbreviation) %>% 
  summarise(numero_sp = sum(species_number))

ggbarplot(data = da_fa2,
            x = "state_abbreviation",
            y = "numero_sp",
            fill = "state_abbreviation",
            color = "state_abbreviation",
            rug = TRUE,
            label = FALSE,
            add_density = TRUE,
            xlab = "Estado",
            ylab = "Número de espécies")

ggsave("ggpubr_fa_state.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# 35 Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

da_fa3 <-da %>% 
  dplyr::select("coordinate_precision", "species_number") %>%      na.omit() %>% group_by(coordinate_precision) %>% 
  summarise(numero_sp = sum(species_number))

ggbarplot(data = da_fa3,
            x = "coordinate_precision",
          y = "numero_sp",
            fill = "coordinate_precision",
          color = "coordinate_precision",
            rug = TRUE,
            add_density = TRUE,
            xlab = "Precisão do GPS",
            ylab = "Número de espécies")

ggsave("ggpubr_fa_precision.tiff", wi = 20, he = 15, un = "cm", dpi = 300)


# Questões 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46 estão na parte de QGIS

# 47 Refaça os passos anteriores do QGIS (exercídios 37 a 46) utilizando as funções do pacote sf, raster e fasterize.


# Diretório
setwd("C:/Users/sergi/Desktop/07_exercicios")

# Importando os dados - 37
usos2<-sf::st_read("PE_2611606_USO_corrigido_topologia.shp", quiet = TRUE)


# Selecionando só o uso florestal - 38
uso_flo <- usos2 %>% 
  dplyr::filter(usos2$CLASSE_USO == "formação florestal")

plot(uso_flo[5],col="green")

st_write(uso_flo,"PE_2611606_USO_topologia_floresta2.shp")# Salvando

# Simplificação - 40
flo_uni<-sf::st_cast(uso_flo, "POLYGON") 

plot(flo_uni[5],col="orange")

st_write(flo_uni,"PE_2611606_USO_topologia_floresta_unico2.shp")

# Criar coluna na tabela de atributos com o nome id - 41
flo_uni2 <- flo_uni %>% mutate(id = seq(1:nrow(flo_uni)))

# Apague as outras colunas, deixando apenas a coluna id - 42
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


# 48 Importe os arquivos raster gerados (com os valores de área, área core e área de borda) junto com o arquivo de DEM usado anteriormente num stack raster::stack()

flo_stack<-stack(molde,flo_ras_unico4,flo_ras_core,flo_ras_edg)

plot(flo_stack)


# 49 Extraia os valores desses quatro rasters, compondo os mesmos num tibble. Por fim, relacione esses valores das colunas (área, área core e área de borda - use o log10 para essas colunas, pois as unidades são diferentes) com o DEM. Faça gráficos de dispersão (scatterplot). Há algum padrão? Discuta brevemente

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

grid.arrange(gg_flo,gg_core,gg_edge) # Em todos os exemplos, existe uma tendência de maior proporção áreas florestadas em locais de maiores elevações. Isso pode ser o reflexo da tendência histórica do desmatamento e crecimento urbano serem voltados principalmente para áreas planas, de fácil acesso.
