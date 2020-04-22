##### Atividade avaliativa da disciplina: 
### Introdução ao geoprocessamento para Etnobiologia e Conservação da Biodiversidade  
### Nome: Natália Ferreira
### Data 02/12/2019

# Pacotes ------------------------------

library(tidyverse)
library(purrr)
library(ggplot2)
library(gridExtra)
library(sf)
library(raster)
library(fasterize)
library(ggpubr)


##3 - Usar uma função para remover todos os objetos possivelmente criados e armazenados no ambiente (environment) antes de iniciar

rm(list = ls()) 

##4 - Vamos começar com alguns cálculos simples:

(2 * 5) - (3 ^ 2)

##5 - Mais alguns cálculos simples:

log10(10) + log(100) * log2(1000) 

##6 - Calcular o fatorial de 10 e Atribuir ao objeto "fa_10" Em seguida, tirar a raiz quadrada desse objeto, atribuindo à outro objeto "fa_10_rq"
##fatorial de 10 (fa_10) e raiz quadrada (fa_10_rq)

fa_10 <- factorial(10)
fa_10_rq <- sqrt(fa_10)

##7 - Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas 

vel.med <- 400 / 3.5
vel.med

##8 - Crie uma sequência unitária de 0 à 10 - Atribua à um objeto chamado "seq_10" Some os elementos desse objeto cumulativamente, atribuindo ao objeto "seq_10_sum"

seq_10 <- seq(1:10)
seq_10_sum <- sum(seq_10)

##9 - Crie uma sequência de 0 à 50, espaçada de 5 em 5. Atribua à um objeto chamado "seq_50" Repita os elementos desse objeto 10 vezes sequencialmente atribuindo ao objeto "seq_50_rep_times"

seq_50 <- seq(0, 50, by = 5)
seq_50_rep_times <- rep(seq_50, 10)

##10 - Mega-Sena 

mega <- list(NULL)
for (i in 1:8) {
  mega <- list(sample(1:60, 6))
  print(mega)
}

##11 - Amostragens aleatórias - Simule o resultado de 25 jogadas de um dado de 12 lados 
for (i in 1:25) {
  dado <- sample(1:12, 1)
  print(paste("Resultado da jogada", i, "foi", dado))
}

##12 - Crie um vetor chamado "lo" para descrever 100 locais de amostragem. Formato: Formato 1 = local_1...

lo <- paste("local", 1:100, sep = "_")
lo

##13 - Formato 2 = local_001  
lo <-
  c(
    paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = "")
  )

##14 - Crie um fator chamado "tr", com dois níveis ("cont" e "trat"). Para descrever 100 locais de amostragem, 50 de cada tratamento. O fator deve ter esse formato: cont, cont...trat, trat...

tr <- as.factor(c(rep("cont",50),rep("trat",50)))

##15 - Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ma <- matrix(sample(0:10, 1000, replace = T), 100, byrow = F)

#### Tidyverse ------------------------------

##16 - Reescreva essa operação utilizando pipes %>%: (log(exp(sqrt(rpois(100, 5)))))

rpois(100, 5) %>% sqrt() %>% exp() %>% log() %>% max()

##17 - Reescreva essa operação removendo os pipes %>%: rnorm(100) %>% exp() %>% log10() %>% min() %>%sqrt()

sqrt(min(log10(exp(rnorm(100)))))

##18 - Reescreva essa operação utilizando pipes %>% e a função magrittr::divide_by() round(mean(sum(1:10)/3), digits = 1)

sum(1:10) %>% magrittr::divide_by(3) %>% mean() %>% round(1)

##19 - Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao objeto da, utilizando o formato tidyverse
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
da2 <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

##20 - Utilize a função tibble::glimpse para verificar as colunas desses dados

glimpse(da)
glimpse(da2)

##21 - Combine as colunas country, state, state_abbreviation, municipality, site, em uma coluna chamada local_total separadas por ,,atribuindo o resultado a um novo objeto

da3 <- da2 %>%
  unite(local_total,
        c(country, state, state_abbreviation, municipality, site),
        sep = ",")

##22 - Separe a coluna passive_methods em outras colunas, atribuindo o resultado a um novo objeto

da4 <- da3 %>% separate(passive_methods, c("pmchar1","pmchar2"))

##23 - Retire as linhas com NA da coluna year_start atribuindo o resultado a um novo objeto

da5 <- da4 %>% drop_na(year_start)

##24 - Selecione todas as colunas que contenham method, atribuindo o resultado a um novo objeto

da6 <- da5 %>% select(ends_with("methods"))

##25 - Faça um histograma da coluna species_number 

hist(da5$species_number)

##26 - Adicione essas novas colunas alt_log, tem_log e pre_log, que são a operação log10 das colunas altitude, temperature e precipitation e atribua ao mesmo objeto da utilizando o formato 

da5 <-
  da5 %>% mutate(alt_log = log(altitude),
                 tem_log = log(temperature),
                 pre_log = log(precipitation))

##27 - Ordene os dados em forma decrescente pela coluna altitude atribuindo o resultado a um novo objeto 

da7 <- da5 %>% arrange(desc(altitude))

##28 - Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm atribuindo o resultado a um novo objeto 

da8 <-
  da7 %>% filter(altitude > 1000,
                 temperature < 15 |
                   precipitation > 1000 &  precipitation <= 1500)

##29 - Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto 
sam <- da5 %>% filter(species_number > 15) %>% sample_n(200)

##30 - Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr

range(da8$species_number, na.rm=TRUE)
range(da8$altitude, na.rm=TRUE)
range(da8$temperature,  na.rm=TRUE)
range(da8$temperature,  na.rm=TRUE)

##31 - Cometi um grave erro no data paper de anfíbios! Algo relacionado à colunas com dados separados com vírgulas, aí quando se abre num .csv (separado por vírgulas) dá ruim... Ache uma solução através das funções do tidyverse e explique nos termos do formato tidyr porque esses dados estão errados. 

dd_cor <- read_csv2("ATLANTIC_AMPHIBIANS_sites.csv")

# A função separa as colunas utilizando ";" ponto e vírgula

##32 - Gere gráficos relacionando o número de espécies e as variáveis altitude, temperature, precipitation (Exportando cada um deles). Existe alguma relação?

g_alt <-
  ggplot(da8, aes(x = species_number, y = altitude)) + geom_point(size =
                                                                    2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Altitude") + theme_bw()

g_temp <-
  ggplot(da8, aes(x = species_number, y = temperature)) + geom_point(size =
                                                                       2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Temperatura") + theme_bw()

g_prec <-
  ggplot(da8, aes(x = species_number, y = precipitation)) + geom_point(size =
                                                                         2.5) + geom_smooth(method = "lm") + xlab("Número de espécies") + ylab("Precipitação") + theme_bw()

grid.arrange(g_alt, g_temp, g_prec) 

# De acordo com os gráficos não há nenhuma relação entre o número de espécies e as variáveis 

##33 - Gere gráficos  mostrando a frequência absoluta de amostragem em cada estado, inverta seu gráfico para a posição horizontal e exporte. 

names(da2)

dd_graf2 <-
  da2 %>% 
  dplyr::select("state_abbreviation", "effort_months", "species_number") %>%      na.omit() %>% group_by(state_abbreviation) %>% 
  summarise(freq_tt = sum(effort_months),                                                 numero_sp = sum(species_number))

ggplot(dd_graf2, aes(x = state_abbreviation, y = freq_tt)) + geom_bar(stat = "identity") + coord_flip()

ggsave("hist_state.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.pdf", wi=20,he = 15, un="cm", dpi=600)

##34 - Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte

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

##35 - Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte

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



#### QGIS ------------------------------

### 36 a 46 no QGIS



### Geoprocessamento no R ------------------------------

###37 - Refaça os passos anteriores do QGIS (exercícios 37 a 46) utilizando as funções do pacote sf, raster e fasterize.
## Importando os dados
uso<-sf::st_read("PE_2611606_USO_topologia_floresta.shp", quiet = TRUE)


###38 - Selecionando somente uso florestal

uso_flo <- uso %>% 
  dplyr::filter(uso$CLASSE_USO == "formação florestal")

plot(uso_flo[5],col="green")

st_write(uso_flo,"PE_2611606_USO_topologia_floresta2.shp")

### Salvar -------------------------------------

### 40 - Multipartes para partes simples 

flo_uni<-sf::st_cast(uso_flo, "POLYGON") 

plot(flo_uni[5],col="hot pink")

st_write(flo_uni,"PE_2611606_USO_topologia_floresta_unico.shp")


### 41 - Criar coluna na tabela de atributos com o nome "id"

flo_uni2 <- flo_uni %>% mutate(id = seq(1:nrow(flo_uni)))

### 42 - APague todas as colunas exceto a "id"

flo_uni3 <- flo_uni2 %>% dplyr::select("id")

### 43 -Calcular a área (decimal) de cada fragmento em hectares chamando a coluna de "a_frag_ha"

flo_uni4 <- flo_uni3 %>% mutate(a_frag_ha = sf::st_area(flo_uni3) / 10000)

### 44 - Criar um buffer negativo (-60) com calculo de area

flo_buf <- flo_uni4 %>% 
  sf::st_buffer(-60) %>% mutate(a_core_ha = sf::st_area(.) / 10000)

flo_buf

plot(flo_buf)

st_write(flo_buf,"PE_2611606_USO_topologia_floresta_unico_core60m.shp")

### 45 - Diferença simétrica 

flo_edg <-
  flo_uni4 %>% sf::st_difference(sf::st_combine(flo_buf)) %>% mutate(a_edge_ha = sf::st_area(.) / 10000) 

plot(flo_edg[2],col="blue")

st_write(flo_edg,"PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

### 46 - Criandos rasters - usando um raster como base

molde <- raster::raster("Mosaico_alinhado.tif")
molde

flo_ras_unico4 <-fasterize::fasterize(sf = flo_uni4%>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"),raster = molde,field = "a_frag_ha")

plot(flo_ras_unico4)

writeRaster(flo_ras_unico4, "PE_2611606_USO_topologia_floresta_unico",format = "GTiff",overwrite=TRUE)

flo_ras_core <-fasterize::fasterize(sf = flo_buf %>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"), raster = molde,field ="a_core_ha")

plot(flo_ras_core)

writeRaster(flo_ras_core, "PE_2611606_USO_topologia_floresta_unico_core60m",format = "GTiff",overwrite=TRUE)

flo_ras_edg <-fasterize::fasterize(sf = flo_edg %>%dplyr::filter(!st_is_empty(.)) %>%sf::st_cast("MULTIPOLYGON"),raster = molde,field = "a_edge_ha")

plot(flo_ras_edg)

writeRaster(flo_ras_edg,"PE_2611606_USO_topologia_floresta_unico_edge60m",format = "GTiff",overwrite=TRUE)

### 48 - Importe os arquivos raster gerados (com os valores de area, area core e Area de borda) junto com o arquivo de DEM usado anteriormente num stack raster::stack()

flo_stack<-stack(molde,flo_ras_unico4,flo_ras_core,flo_ras_edg)

plot(flo_stack)

### 49 - Extraia os valores desses quatro rasters, compondo os mesmos num tibble. Por fim, relacione esses valores das colunas (área, area core e área de borda - use o log10 para essas colunas, pois as unidades são diferentes) com o DEM
#Faça gráficos de dispersão (scatterplot)

names(flo_stack)<-c("molde","unico","core","borda")

st_ext <- flo_stack %>% 
  raster::values()%>%
  tibble::as_tibble()

st_ext

gg_flo <- ggplot(data = st_ext)+
  aes(x = molde, y = log10(unico + 1)) +
  geom_point() +
  labs(x = "Elevacao", y = "Area") +
  stat_smooth(method = "lm") + 
  theme_bw()


gg_core <- ggplot(data = st_ext, aes(x = molde, y = log10(core + 1))) +
  geom_point() +
  labs(x = "Elevacao", y = "Area core") +
  stat_smooth(method = "lm") +
  theme_bw()

gg_edge <- ggplot(data = st_ext, aes(x = molde, y = log10(borda + 1))) +
  geom_point() +
  labs(x = "Elevacao", y = "Area de borda") +
  stat_smooth(method = "lm") +
  theme_bw()

grid.arrange(gg_flo,gg_core,gg_edge)

##### Existe uma relação entre elevação e área florestada, uma vez que a cidade cresce mais em áreas planas, áreas mais elevadas, são de dificil acesso, logo há uma tendência de maior redução de área florestal em áreas planas comparadas a áreas mais altas. 


### 50 - 50. Se Recife é o mundo e nós queremos achar soluções para os problemas do mundo, comecemos com Recife  (No Word)