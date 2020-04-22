#Exercícios correspondentes a Disciplina de Geoprocessamento 
#Professor: Maurício Vancine


#Universidade Federal Rural de Pernambuco (UFRPE)
#Programa de Pós-graduação em Etnobiologia e Conservação da Natureza
#Nome: Cássio Henrique Carvalho da Silva
#Data: 11/11/2019


library(tidyverse)
library(purrr)
library(ggplot2)
library(gridExtra)
library(sf)
library(raster)
library(fasterize)
library(ggpubr)


#------- Seguindo dando um 'check' nos exerícios aqui, seguindo ordem numérica =)-----#

## 1. Crie um novo script na pasta `07_exercicios`, com o nome `script_r_exercicios.R`. 
#Envie esse script com as resoluções dos exercícios

# - Done -#


## 2. Crie um cabeçalho descritivo para esse script: objetivo, nome e data

# - Done -#

## 3. Use uma função para remover todos os objetos possivelmente criados e armazenados 
#no ambiente (*environment*) antes de iniciar

rm(list = ls())  # - Done -#


## 4. Vamos começar com alguns cálculos simples:

(2 * 5) - (3 ^ 2)


## 5. Mais alguns cálculos simples:
log10(10) + log10(100) * log2(1000)


## 6. Ainda mais alguns cálculos simples:

#`Calcule o fatorial de 10: 10!`

#`Atribua ao objeto "fa_10"`

#`Em seguida, tire a raiz quadrada desse objeto, atribuindo à outro objeto "fa_10_rq"`


fa_10 <- (10/factorial(10))
fa_10_rq <- sqrt(fa_10)
fa_10_rq


## 7. Sim, mais alguns cálculos simples:

#`Calcule a velocidade média de um carro que percorreu S = 400 km em t = 3.5 horas`

vm_car <- (400/3.5)
vm_car


## 8. Bora de sequências

#`Crie uma sequência unitária de 0 à 10`

#`Atribua à um objeto chamado "seq_10"`

#`Some os elementos desse objeto cumulativamente, atribuindo ao objeto "seq_10_sum"`

seq_10 <- seq(from = 0, to = 10)

seq_10_sum <- cumsum(seq_10)


## 9. Agora com repetições

#`Crie uma sequência de 0 à 50, espaçada de 5 em 5`

#`Atribua à um objeto chamado "seq_50"`

#`Repita os elementos desse objeto 10 vezes sequencialmente, atribuindo ao objeto 
#"seq_50_rep_times"` 


seq_50 <- seq(from = 0, to = 50, by = 5)
seq_50_rep_times <- rep(seq_50, times = 10)


## 10. Mega-Sena, quem sabe...

#`Escolha 6 números para jogar na Mega-Sena durante um mês (duas vezes por semana)`
#`Atribua esses resultados à uma lista, de modo que cada elemento contenha os 6 números`
#`Lembrando: valores da Mega-Sena vão de 1 a 60`

megasena<- list(a = c(1,13,22,48,55,60), b = c(2,14,23,49,56,60))
megasena


## 11. Amostragens aleatórias

#`Einstein disse que Deus não joga dados, mas o R joga!`
#`Simule o resultado de 25 jogadas de um dado de 12 lados (sim, no RPG tem esse dado)

sample(1:12, size = 25, replace = TRUE)


## 12. Crie um vetor chamado "lo" para descrever 100 locais de amostragem. 
#O vetor deve ter esse formato:`local_1, local_2, local_3, ...., local_100`

prefix <-  ("local_")
suffix <- (1:100)
lo <- paste(prefix, suffix, sep = "")


## 13. Crie um vetor chamado "lo" para descrever 100 locais de amostragem. 
#Mas agora o vetor deve ter esse formato:
#`local_001, local_002, local_003, ...., local_100`

lo <-c(paste("local_00", 1:9, sep = ""),
    paste("local_0", 10:99, sep = ""),
    paste("local_", 100, sep = ""))
lo


# Exercícios

## 14. Crie um fator chamado "tr", com dois níveis ("cont" e "trat") para 
# descrever 100 locais de amostragem, 50 de cada tratamento. 
# O fator deve ter esse formato:`cont, cont, cont, ...., cont, trat, trat, trat, ...., trat`

tr <- as.factor(c(rep("cont",50),rep("trat",50)))

## 15. Crie uma matriz chamada "ma", da disposição de um vetor composto por 10000 valores 
# aleatórios entre 0 e 10. A matriz deve conter 100 linhas e ser disposta por colunas

ma <- matrix(sample(0:10, 1000, replace = T), 100, byrow = F)
ma

library(tidyverse)
## 16. Reescreva essa operação utilizando pipes `%>%`:
#`max(log(exp(sqrt(rpois(100, 5)))))
rpois(100, 5) %>% 
  sqrt() %>% 
  exp() %>% 
  log() %>% 
  max()

## 17. Reescreva essa operação removendo os pipes `%>%`:
#```{r, eval = FALSE}
#rnorm(100) %>%
# exp() %>%
# log10() %>% 
# min() %>%
# sqrt()

sqrt(min(log10(exp(rnorm(100)))))

## 18. Reescreva essa operação utilizando pipes `%>%` e a função `magrittr::divide_by()`
#```{r, eval = FALSE} round(mean(sum(1:10)/3), digits = 1)

sum(1:10) %>% magrittr::divide_by(3) %>% mean() %>% round(1)


## 19. Importe o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) e atribua ao 
# objeto `da`, utilizando o formato tidyverse

da_sp <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv")
da_st <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da_st_pe <- readr::read_csv("ATLANTIC_AMPHIBIAN_sites_pernambuco.csv")

## 20. Utilize a função `tibble::glimpse` para verificar as colunas desses dados

tibble::glimpse(da_sp)
tibble::glimpse(da_st)
tibble::glimpse(da_st_pe)

## 21. Combine as colunas `country`, `state`, `state_abbreviation`, `municipality`, 
#`site`, em uma coluna chamada `local_total` separadas por `,`, atribuindo o resultado 
#a um novo objeto, utilizando o formato tidyverse

da_combine <- da_st_pe %>%
  unite(local_total,
        c(country, state, state_abbreviation, municipality, site),
        sep = ",")

## 22. Separe a coluna `passive_methods` em outras colunas (mesmo com o erro...), 
# atribuindo o resultado a um novo objeto, utilizando o formato tidyverse

da_sep <- da_st_pe %>% 
  separate(passive_methods, c("pmchar1","pmchar2"))

## 23. Retire as linhas com NA da coluna `year_start`, atribuindo o resultado a um 
# novo objeto, utilizando o formato tidyverse

da_sem_na <- da_sep %>% 
  drop_na(year_start)

## 24. Selecione todas as colunas que contenham `method`, atribuindo o resultado a 
# um novo objeto, utilizando o formato tidyverse

da_method <- da_sem_na %>% 
  select(ends_with("methods"))


## 25. Faça um histograma da coluna `species_number` utilizando o formato tidyverse

hist(da_sem_na$species_number)

## 26. Adicione essas novas colunas `alt_log, tem_log e pre_log`, que são a operação 
#`log10` das colunas `altitude,	temperature e	precipitation` e atribua ao mesmo objeto 
#`da` utilizando o formato tidyverse

da_sem_na <-
  da_sem_na %>% mutate(alt_log = altitude,
                 tem_log = temperature,
                 pre_log = precipitation)

## 27. Ordene os dados em forma decrescente pela coluna `altitude`, atribuindo o 
# resultado a um novo objeto utilizando o formato tidyverse
da_order <- da_sem_na %>% 
  arrange(desc(altitude))

## 28. Filtre as linhas com `altitude` maior que 1000 mm, `temperature` menor que 15 ºC 
# ou `precipitation` maior que 1000 mm e menor ou igual que 1500 mm, atribuindo 
# o resultado a um novo objeto utilizando o formato tidyverse

da_filter <- da_order %>% 
  filter(altitude > 1000, temperature < 15 | precipitation > 1000 &  precipitation < 1500)


## 29. Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, 
#atribuindo o resultado a um novo objeto utilizando o formato tidyverse

da_amostras <- da_sem_na %>%
  filter(species_number > 15) %>%
  sample_n(200)

## 30. Calcule o `range` sem os NAs, para as colunas `species_number, altitude, 
# temperature, precipitation` usando o pacote `purrr` utilizando o formato tidyverse

#[*] https://gge-ucd.github.io/R-DAVIS/lesson_purrr_tutorial.html


range(da_filter$species_number, na.rm=TRUE)
range(da_filter$altitude, na.rm=TRUE)
range(da_filter$temperature,  na.rm=TRUE)
range(da_filter$temperature,  na.rm=TRUE)

## 31. Cometi um grave erro no data paper de anfíbios... Algo relacionado à colunas 
#com dados separados com vírgulas, aí quando se abre num .csv (separado por vírgulas) 
#dá ruim... Ache uma solução através das funções do tidyverse e explique nos termos 
#do formato `tidyr` porque esses dados estão errôneos

#https://r4ds.had.co.nz/tidy-data.html

#https://blog.rstudio.com/2014/07/22/introducing-tidyr/
  
#### Aqui não deu ruim ??? Rodou normal ####


## 32. Gere gráficos no `ggplot2` relacionando o número de espécies e as variáveis 
#`altitude, temperature, precipitation`, exportando cada um deles, utilizando o formato 
#tidyverse. Responda: existe alguma relação?

library(ggplot2)

ggda_altitude <- ggplot(da_filter, aes(x = species_number, y = altitude)) + 
  geom_point(size =2.5) + 
  geom_smooth(method = "lm") + 
  xlab("Número de espécies") + 
  ylab("Altitude") + theme_bw()

ggda_temper <- ggplot(da_filter, aes(x = species_number, y = temperature)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method = "lm") + 
  xlab("Número de espécies") + 
  ylab("Temperatura") + theme_bw()

ggda_precipit <- ggplot(da_filter, aes(x = species_number, y = precipitation)) + 
  geom_point(size = 2.5) + 
  geom_smooth(method = "lm") + 
  xlab("Número de espécies") + 
  ylab("Precipitation") + theme_bw()


library(gridExtra)
grid.arrange(ggda_altitude, ggda_temper, ggda_precipit) 
# Aparentemente não há relação, de acordo com os dados exibidos pelos gráficos


## 33. Gere gráficos no `ggplot2` mostrando a frequência absoluta de amostragem em cada 
# estado e exporte. Ah, inverta seu gráfico para a posição horizontal... e exporte 
# utilizando o formato tidyverse

names(da_st)

ggda_fa <- da_st %>% 
  dplyr::select("state_abbreviation", "effort_months", "species_number") %>%
  na.omit() %>% group_by(state_abbreviation) %>% 
  summarise(freq_tt = sum(effort_months),                                                 numero_sp = sum(species_number))

ggplot(ggda_fa, aes(x = state_abbreviation, y = freq_tt)) + 
  geom_bar(stat = "identity") + coord_flip()

ggsave("hist_state.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_state.pdf", wi=20,he = 15, un="cm", dpi=600)


## 34. Gere gráficos no `ggpubr` relacionando o número de espécies e os estados e exporte

library(ggpubr)

ggbarplot (ggda_fa, x = "state_abbreviation", y = "numero_sp", fill = "state_abbreviation", 
           color = "state_abbreviation", palette = c("Harmonic"),
           label = FALSE, lab.size = 5, xlab = "Estados", ylab = "Número de espécies",
           legend = "none", orientation = "horiz")

ggsave("hist_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_sp_ggpubr.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("hist_sp_ggpubr.pdf", wi=20,he = 15, un="cm", dpi=600)


## 35. Gere gráficos no `ggpubr` relacionando o número de espécies e a precisão do GPS e exporte


ggda_sp_precision <- da_st %>% 
  dplyr::select("species_number", "coordinate_precision") %>% 
  na.omit() %>% group_by(coordinate_precision) %>%
  summarise(numero_sp = sum(species_number))

ggbarplot(data= ggda_sp_precision, x = "coordinate_precision", y = "numero_sp", 
          fill = "coordinate_precision", color = "coordinate_precision",
          palette = c("PuOr"), label = FALSE, lab.size = 5, xlab = "Precisão do GPS",
          ylab = "Número de espécies", legend = "none", order = c("gms","gm","dd","utm"))

ggsave("gps_sp_ggpubr.png", wi=20,he = 15, un="cm", dpi=600)
ggsave("gps_sp_ggpubr.svg", wi=20,he = 15, un="cm", dpi=600)
ggsave("gps_sp_ggpubr.pdf", wi=20,he = 15, un="cm", dpi=600)

  
### 36. Crie uma pasta dentro da pasta `07_exercicios`, com o nome de 
#`exercicios_qgis`. Crie um projeto novo chamado `proj_qgis_exercicio_utm_25s_sirgas2000`.
# Todos os arquivos processados devem ser salvos nessa pasta. No final, envie essa pasta
# junto com o projeto do QGIS


#_____DONE______#


### 37.1. Importe o arquivo vetorial de uso e cobertura da terra que você rodou a 
#topologia (correção dos erros de topologia). No meu caso, salvei com esse nome: 
#`PE_2611606_USO_topologia.shp`

#______DONE____#

#--------------------Parte de Geoprocessamento no R---------------------------#
library(tidyverse)
# 37.2. Importar o shapefile
topologia <- sf::st_read("PE_2611606_USO_topologia_floresta.shp")

# 38. Filtrar formação florestal 
forest <- topologia %>% filter(CLASSE_USO == "formação florestal")
forest
# 39. Tratar as multipartes

parte_unica <- forest %>% sf::st_cast("POLYGON")
parte_unica

# 40. Criar coluna id integer
id <- parte_unica %>% mutate(id = seq(1:nrow(multi))) #erro - até tentei fazer parecido como com o que Reginaldo fez, mas não rolou



#_____________________________________________________________________________________#

#Infelizmente não consegui continuar a partir daqui, professor :(
#Até tentei ver em grupo com os alguns da turma, mas não consegui resolver. Acho que é algo nos meus dados ou não tõ sabendo importar certo



# Importar os arquivos raster com stack


uni <- raster("PE_2611606_USO_topologia_floresta_unico.tif")
core <- raster("PE_2611606_USO_topologia_floresta_unico_core60m.tif")
edge <- raster("PE_2611606_USO_topologia_floresta_unico_edge60m.tif")

stack <- raster::stack(uni, core, edge) #Error in compareRaster(x) : different extent

#__________________________________GameOver______________________________________#





