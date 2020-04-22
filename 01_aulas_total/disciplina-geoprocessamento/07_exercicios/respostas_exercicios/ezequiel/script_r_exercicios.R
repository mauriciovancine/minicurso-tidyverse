#UNIVERSIDADE FEDERAL RURAL DE PERNAMBUCO
#PROGRAMA DE PÓS-GRADUAÇÃO EM ETNOBIOLOGIA E CONSERVAÇÃO DA NATUREZA
#DISCIPLINA DE GEOPROCESSAMENTO 
#MESTRANDO EZEQUIEL LEANDRO DA SILVA JÚNIOR
#ATIVIDADE DE CONCLUSÃO DA DISCIPLINA DE GEOPROCESSAMENTO
#28 DE NOVEMBRO DE 2019

##EXERÍCIO NÃO REALIZADOS = 13,33,34,35,46,47,48 e 49  :'(

##EXERÍCIO 04
( 2 * 5) - ( 3 ^ 2 )

#EXERÍCIO 05
log10(10) + log(100) * log2(1000)

#EXERÍCIO 06
factorial(10)
"fa_10" <- factorial(10)
fa_10
sqrt(fa_10)
"fa_10_rq"<-sqrt(fa_10) 
fa_10_rq

#EXERÍCIO 07
"vel_km"<- 400
vel_km
"time_min"<- 3.5
time_h
"resposta_km_h" <- 400/3.5
resposta_km_h

#EXERÍCIO 08
0:10
seq_10<-0:10
seq_10
"seq_10_sum"<-cumsum(seq_10)
seq_10_sum

#EXERÍCIO 09
seq_50<-seq(from=0,to=50,by=5)
rep(seq_50, times=10)
"seq_50_rep_times"<-rep(seq_50, times=10)

#EXERÍCIO 10
list_megasena<-list(elem1=6,8,11,13,17,23, elem2=8,6,13,23,17,11, elem3=11,17,23,13,6,8, elem4=17,11,13,23,8,6, elem5=23,13,8,11,6,17, elem6=13,17,6,11,23,8, elem7=8,23,11,6,17,13, elem8=23,6,17,8,13,11)

##EXERCÍCIO 11
dado_12 <- 1:12
dado_12
resposta <- sample(dado_12, 25, replace = TRUE, prob = NULL)
resposta

##EXERCÍCIO 12
lo<-paste("local_", 1:100)
lo



##EXERCÍCIO 14
fa_tr <- factor(x = c("cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "cont", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat", "trat"), levels = c("cont", "trat"))
fa_tr

##EXERCÍCIO 15
ve <- 1:10
ve
ma <- matrix(data = ve, nrow = 1000, ncol = 10, byrow = TRUE)
ma

##EXERCÍCIO 16
library(tidyverse)

max(log(exp(sqrt(rpois(100, 5)))))
rpois(100, 5) %>% 
sqrt() %>% 
exp() %>% 
log() %>% 
max()

##EXERCÍCIO 17
rnorm(100) %>%
exp() %>%
log10() %>% 
min() %>%
sqrt()
sqrt(min(log10(exp(rnorm(100)))))

##EXERCÍCIO 18
round(mean(sum(1:10)/3), digits = 1)
1:10 %>% 
  sum %>% 
  magrittr::divide_by(3) %>% 
  round(digits = 1)
##EXERCÍCIO 19
setwd("C:/Users/EzequielLeandro/OneDrive/Ethnobiology and Conservation/Mestrado/Disciplina/2° Semestre/Geoprocessamento/GIT/disciplina-geoprocessamento/03_dados/00_tabelas")
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

##EXERÍCIO 20
glimpse(da)

##EXERÍCIO 21
da_unite <- da %>% 
  unite("local_total",country, state, state_abbreviation, municipality, site, sep = ",")
da_unite$local_total

##EXERÍCIO 22
da_separate <- da %>% 
  separate("passive_methods", kamehameha, kaioken, remove = FALSE)
da_separate[, kamehameha, kaioken]

##EXERÍCIO 23
da_drop_na <- da %>% 
  drop_na(year_start)
da_drop_na

##EXERÍCIO 24
da_select <- da %>% 
  select(active_methods, passive_methods, complementary_methods)
da_select

##EXERÍCIO 25
glimpse(da)
plot("species_number", data = da)
library(ggplot2)
ggplot(data = da) + aes(effort_months, species_number) + geom_point()
hist(da$species_number)
hist(da$species_number,
     col = "gray50",
     border = "gray")
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti")
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr")
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr",
     br = 50)
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr",
     br = 50,
     cex.main = 2.5,
     cex.lab = 2.2,
     cex.axis = 2)
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr",
     br = 50,
     cex.main = 2.5,
     cex.lab = 2.2,
     cex.axis = 2,
     prob = TRUE)
lines(density(da$species_number))

# fecha o arquivo
dev.off()

# ggplot2
# data
ggplot(data = da)

# aes
ggplot(data = da) +
  aes(species_number)

# geom
ggplot(data = da) +
  aes(species_number) +
  geom_histogram()

# density
ggplot(data = da) +
  aes(species_number) +
  geom_density()

# change
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10)

# theme
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, alpha = .5) +
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# density
ggplot(data = da) +
  aes(species_number) +
  geom_density(color = "black", fill = "forest green", alpha = .5) + 
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# facet
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_wrap(~ record, ncol = 2, scale = "free_y") +
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# facet
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_grid(record ~ .) +
  labs(x = "Número de Espécies",
       y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# theme_*
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_grid(record ~ .) +
  labs(x = "Número de Espécies",
       y = "Frequência") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# export
ggplot(data = da) +
  aes(species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10,
                 alpha = .5) +
  facet_grid(record ~ .) +
  labs(x = "Número de Espécies",
       y = "Frequência") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
ggsave("histogram_ggplot2.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

# ggpubr
gghistogram(data = da, 
            x = "species_number",
            add = "median",
            fill = "steelblue",
            rug = TRUE,
            add_density = TRUE,
            xlab = "Número de espécies",
            ylab = "Frequeência absoluta")

# export
gghistogram(data = da, x = "species_number",
            add = "median",
            fill = "steelblue",
            rug = TRUE,
            add_density = TRUE,
            xlab = "Número de espécies",
            ylab = "Frequeência absoluta")
ggsave("histogram_ggpubr.tiff", wi = 20, he = 15, un = "cm", dpi = 300)

##EXERÍCIO 26
da_mutate <- da %>% 
  mutate(alt_log = altitude, tem_log = temperature, pre_log = precipitation )
da_mutate

##EXERÍCIO 27
da_arrange <- da %>% 
  arrange(desc(altitude))
da_arrange

##EXERÍCIO 28
da_filter <- da %>% 
  filter(altitude > 1000, temperature < 15, precipitation > 1000)
da_filter

##EXERÍCIO 29
da_filter <- da %>% 
  filter(species_number > 15)
da_filter

da_sample_n <- da_filter %>% 
  sample_n(200)
da_sample_n

##EXERÍCIO 30
da_drop_na <- da %>% 
  drop_na()
da_drop_na

mean_var_da <- da_drop_na %>% 
  select(species_number, altitude, temperature, precipitation) %>% 
  map_dbl(var)
mean_var_da

##Exercício 31
setwd("C:/Users/EzequielLeandro/OneDrive/Ethnobiology and Conservation/Mestrado/Disciplina/2° Semestre/Geoprocessamento/GIT/disciplina-geoprocessamento/03_dados/00_tabelas")
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv") %>% 
  da <- iconv(enc2utf8(da),sub="byte")
da
glimpse(da)
##Resposta do exercício 31 - acredito que deva ser algum erro que o pacote string possa resolver, pois ele traz alternativas para manipular a separação dos dados das colunas, com funções que adicionam ou removem espaços, etc. Entretanto, o erro de "invalid multibyte string, element 19" ainda persistiu, no que não consegui resolver mesmo com a ajuda do Google

##EXERCÍCIO 32 
plot(species_number ~ effort_months, data = da)

# ggplot2
library(ggplot2)
ggplot(data = da) + aes(species_number,altitude) + geom_point()
ggplot(data = da) + aes(species_number,temperature) + geom_point()

##RESPOSTA = QUANTO MAIOR AS VARIÁVEIS, MENOR O NÚMERO DE ESPÉCIES.







