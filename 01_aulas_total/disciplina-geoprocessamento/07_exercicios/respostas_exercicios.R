# Exercícios

### 1. Criem um novo script na pasta...
# script_r_exercicios.R
 
### 2. Criem um cabeçalho descritivo para esse script
# nome
# data

### 3. Removam todos os objetos criados anteriormente
# solucao
### memoria
rm(list = ls())

### 4. Alguns cálculos simples:
# solucao
abs((2 * 5) - (3 ^ 2))

### 5. Alguns cálculos simples:
# solucao
log10(10) + log(100) * log2(1000)

### 6. Alguns cálculos simples:
# solucao
fa_10 <- factorial(10)
fa_10

fa_10_rq <- sqrt(fa_10)
fa_10_rq

### 7. Alguns cálculos simples:
# solucao
s <- 400
s

t <- 3.5
t

v <- s / t
v 

### 8. Sequências
# solucao
seq_10 <- 0:10
seq_10

seq_10_sum <- cumsum(seq_10)
seq_10_sum

### 9. Repetições
# solucao
seq_50 <- seq(0, 50, by = 5)
seq_50

seq_50_rep_times <- rep(seq_50, times = 10)
seq_50_rep_times

### 10. Amostragens aleatórias
mega_num <- sample(1:60, 6)
mega_num

### 11. Amostragens aleatórias
dado_25 <- sample(1:6, 25, rep = TRUE)
dado_25

### 12. Criem um vetor chamado "lo" para descrever 100 locais de amostragem. O vetor deve ser dessa forma:
# solucao
lo <- paste("local", 1:100, sep = "_")
lo

### 13. Criem um vetor chamado "lo" para descrever 100 locais de amostragem. O vetor deve ser dessa forma:
# solucao
lo_un <- paste("local_00", 1:9, sep = "")
lo_un

lo_de <- paste("local_0", 10:99, sep = "")
lo_de

lo <- c(lo_un, lo_de, "local_100")
lo

### 13. Criem um vetor chamado "lo" para descrever 100 locais de amostragem. O vetor deve ser dessa forma:
# solucao 2
lo_un <- paste("local", 1:9, sep = "_00")
lo_un

lo_de <- paste("local", 10:99, sep = "_0")
lo_de

lo <- c(lo_un, lo_de, "local_100")
lo

### 14. Criem um fator chamado "tr", com dois níveis ("cont" e "trat") para descrever 100 locais de amostragem, 50 de cada tratamento. O fator deve ser dessa forma:
# solucao
re <- rep(c("cont", "trat"), each = 50)
re

tr <- factor(re, c("cont", "trat"))
tr

### 14. Criem um fator chamado "tr", com dois níveis ("cont" e "trat") para descrever 100 locais de amostragem, 50 de cada tratamento. O fator deve ser dessa forma:
# solucao
tr <- factor(rep(c("cont", "trat"), each = 50),
             c("cont", "trat"))
tr

### 15. Criem uma matriz chamada "ma", da disposição de um vetor composto por 100 valores aleatórios entre 0 e 10. A matriz deve conter 10 linhas e ser disposta por linhas
# solucao
ve <- sample(0:10, 100, rep = TRUE)
ve

ma <- matrix(ve, 10, 10, byrow = FALSE)
ma

### 16. Reescreva utilizando pipes
# solucao
rpois(100, 5) %>% 
  sqrt %>% 
  exp %>% 
  log %>% 
  max

### 17. Reencreva sem utilizar pipes
# solucao
sqrt(min(log10(exp(rnorm(100)))))

### 18. Reencreva utilizando pipes, utiliza a função magrittr::divide_by()
# solucao
library(magrittr)

1:10 %>% 
  sum %>% 
  divide_by(3) %>% 
  round(1)

### 19. Importem o data paper de anfíbios ATLANTIC AMPHIBIANS (.csv) usando o formato tidyverse
# solucao
# diretorio
setwd("/home/mude/data/github/disciplina-geoprocessamento/03_dados/00_tabelas")

# import sites
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

### 20. Use a função tibble::glimpse para verificar as colunas dos dados
# solucao
tibble::glimpse(da)

### 21. Junte as colunas country,	state,	state_abbreviation,	municipality,	site, separadas por , , com o nome de local_total, atribuindo o resultado a um novo objeto
# solucao
da_unite <- da %>% 
  tidyr::unite("local_total", country:site, sep = ",")
da_unite

### 22. Separe a coluna passive_methods, atribuindo o resultado a um novo objeto
# solucao
da_separate <- da %>% 
  tidyr::separate(passive_methods, unique(da$passive_methods))
da_separate

### 23. Retire as linhas com NA da coluna year_start, atribuindo o resultado a um novo objeto
# solucao
da_drop_na <- da %>% 
  tidyr::drop_na(year_start)
da_drop_na


### 24. Selecione as colunas que contenham method, atribuindo o resultado a um novo objeto
# solucao
da_select <- da %>% 
  dplyr::select(contains("method"))
da_select

### 25. Faça um histograma da coluna species_number usando o formato tidyverse
# solucao
da %>% 
  dplyr::select(species_number) %>% 
  dplyr::pull() %>% 
  hist

### 26. Adicione novas colunas alt_log, tem_log e pre_log, que são os log10 das colunas altitude,	temperature e	precipitation e atribua ao mesmo objeto da
# solucao
da <- da %>% 
  dplyr::mutate(alt_log = log10(altitude),
                tem_log = log10(temperature),
                pre_log = log10(precipitation))
da

### 27. Ordene os dados em forma decrescente pela coluna altitude, atribuindo o resultado a um novo objeto
# solucao
da_arrange <- da %>% 
  dplyr::arrange(altitude)
da_arrange

### 28. Filtre as linhas com altitude maior que 1000 mm, temperature menor que 15 ºC ou precipitation maior que 1000 mm e menor ou igual que 1500 mm, atribuindo o resultado a um novo objeto
# solucao
da_filter <- da %>% 
  dplyr::filter(altitude > 1000 &
                  temperature < 115 |
                  precipitation > 1000 &
                  precipitation <= 1500)
da_filter

### 29. Amostre 200 linhas aleatoriamente com número de espécies maior que 15 espécies, atribuindo o resultado a um novo objeto
# solucao
da_sample <- da %>% 
  dplyr::filter(species_number > 15) %>% 
  dplyr::sample_n(200)
da_sample

### 30. Calcule o range sem os NAs, para as colunas species_number, altitude, temperature, precipitation usando o pacote purrr
# solucao
da_range <- da %>% 
  dplyr::select(species_number, altitude, temperature, precipitation) %>% 
  purrr::map_df(function(x) range(x, na.rm = TRUE))
da_range

### 31. Cometi um grave erro no data paper. Algo relacionado a colunas com dados separados com vírgulas nas colunas... Ache uma solucao atraves das funcoes do tidyverse e explique nos termos do pacote tidyr porque esses dados estao erroneos
# solucao
da_correcao <- da %>% 
  dplyr::mutate(sampled_habitat = stringr::str_replace_all(sampled_habitat, ",", "|"),
                active_methods = stringr::str_replace_all(active_methods, ", ","|"),
                passive_methods = stringr::str_replace_all(passive_methods, ",", "|"),
                complementary_methods = stringr::str_replace_all(complementary_methods, ",", "|"),
                period = stringr::str_replace_all(period, ",", "|"))
da_correcao

# no formato tidyr cada valor deve ficar numa celula e cada variavel numa coluna

  
### 32. Gere gráficos no ggplot2 relacionando o número de espécies e as colunas altitude, temperature, precipitation, exportando cada um deles
# solucao
# altitude
ggplot(data = da) +
  aes(x = altitude, y = species_number) +
  geom_point(colour = "black", fill = "orange4", size = 5, alpha = .5, pch = 21) +
  labs(x = "Altitude (m)", y = "Número de espécies") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("scatter_numero_especies_altitude.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

### 32. Gere gráficos no ggplot2 relacionando o número de espécies e as colunas altitude, temperature, precipitation, exportando cada um deles
# temperature
ggplot(data = da) +
  aes(x = temperature, y = species_number) +
  geom_point(colour = "black", fill = "brown", size = 5, alpha = .5, pch = 21) +
  labs(x = "Temperatura (ºC)", y = "Número de espécies") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("scatter_numero_especies_temperatura.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

### 32. Gere gráficos no ggplot2 relacionando o número de espécies e as colunas altitude, temperature, precipitation, exportando cada um deles
# precipitation
ggplot(data = da) +
  aes(x = precipitation, y = species_number) +
  geom_point(colour = "black", fill = "darkslateblue", size = 5, alpha = .5, pch = 21) +
  labs(x = "Precipitação (mm)", y = "Número de espécies") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("scatter_numero_especies_precipitacao.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

### 33. Gere gráficos no ggplot2 mostrando a frequência absoluta de amostragem em cada estado e exporte. Ah, inverta seu gráfico... e exporte
# solucao
ggplot(data = da) +
  aes(x = forcats::fct_infreq(state_abbreviation)) +
  geom_bar(fill = "deepskyblue4") +
  labs(x = "Estados", y = "Frequêcia absoluta de amostragem") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("barplot_numero_amostragem_estado.tiff", wi = 20, he = 15, un = "cm", dpi = 300)
  
### 34. Gere gráficos no ggpubr relacionando o número de espécies e os estados e exporte
# solucao
ggboxplot(data = da %>% dplyr::arrange(state_abbreviation), 
          x = "state_abbreviation", 
          y = "species_number",
          add = "jitter", 
          fill = "gray",
          color = "black",
          xlab = "Estados",
          ylab = "Número de espécies",
          legend = "none")
ggsave("box_numero_especies_estados.tiff", wi = 20, he = 15, un = "cm", dpi = 300)
  
### 35. Gere gráficos no ggpubr relacionando o número de espécies e a precisão do GPS e exporte
# solucao
ggboxplot(data = da, 
          x = "coordinate_precision", 
          y = "species_number",
          add = "jitter", 
          fill = "gray",
          color = "black",
          xlab = "Precisão do GPS",
          ylab = "Número de espécies",
          legend = "none")

ggsave("boxplot_numero_especies_gps.tiff", wi = 20, he = 15, un = "cm", dpi = 300)
