##### Excercicios Geoprocessamento #####
########### Sidney Andrade #############
############ 02/12/2019 ################


# Exercicio 3  --------------------------------------------------------------
rm(list = ls())

# Exercicio 4 ---------------------------------------------------------------
abs((2 * 5) - (3 ^ 2))

# Exercicio 5 -------------------------------------------------------------

log10(10) + log(100) * log2(1000)

# Exercicio 6 -------------------------------------------------------------

fa_10 <- factorial(10)
fa_10_rq <- sqrt(fa_10)

# Exercicio 7 -------------------------------------------------------------

S <- 400; t <- 3.5
vm <- S/t
vm

# Exercicio 8 -------------------------------------------------------------

seq_10 <- seq(0,10,1)
seq_10_sum <- cumsum(seq_10)

# Exercicio 9 -------------------------------------------------------------

seq_50 <- seq(0,50,5)
seq_50_rep_times <- rep(seq_50, each = 10)

# Exercicio 10 ------------------------------------------------------------

mega <- seq(1,60,1)
Semana_1 <- list(dia_1 = sample(mega, 6, replace = F ), dia_2 = sample(mega, 6, replace = F ))
Semana_2 <- list(dia_1 = sample(mega, 6, replace = F ), dia_2 = sample(mega, 6, replace = F ))
Semana_3 <- list(dia_1 = sample(mega, 6, replace = F ), dia_2 = sample(mega, 6, replace = F ))
Semana_4 <- list(dia_1 = sample(mega, 6, replace = F ), dia_2 = sample(mega, 6, replace = F ))

lista <- list(Semana_1=Semana_1,Semana_2=Semana_2, Semana_3=Semana_3, Semana_4=Semana_4)
lista

# Exercicio 11 ------------------------------------------------------------
dado <- c(1:12)
sample(dado, 25, replace = T)

# Exercicio 12 ------------------------------------------------------------

lo <- paste("local", 1:100, sep = "_")

# Exercicio 13 ------------------------------------------------------------

l1 <- paste("local", 1:9, sep = "_00")
l2 <- paste("local", 10:99, sep = "_0")
l3 <- "local_100"
lo <- c(l1,l2,l3)

# Exercicio 14 ------------------------------------------------------------

tr <- rep(c("cont", "trat"), each = 50)
tr <- as.factor(tr)
tr

# Exercicio 15 ------------------------------------------------------------
ma <- matrix(sample(seq(0,10,1), replace = 10000), ncol = 100, nrow = 100)

# Exercicio 16 ------------------------------------------------------------
library(tidyverse)
max(log(exp(sqrt(rpois(100, 5)))))

rpois(100, 5) %>% max() %>% log() %>% exp() %>% sqrt()

# Exercicio 17 ------------------------------------------------------------

rnorm(100) %>%
  exp() %>%
  log10() %>% 
  sqrt()

min(log10(exp(sqrt(rnorm(100)))))

# Exercicio 18 ------------------------------------------------------------
round(mean(sum(1:10)/3), digits = 1)

1:10 %>% sum %>%  magrittr::divide_by(3) %>% round(digits = 1)

# Exercicio 19 ------------------------------------------------------------
setwd(choose.dir())
dir()
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")

# Exercicio 20 ------------------------------------------------------------
tibble::glimpse(da)

# Exercicio 21 ------------------------------------------------------------
loctot <- da %>% unite("local_total", country:site, sep = ",")
loctot$local_total
# Exercicio 22 ------------------------------------------------------------

separ <- da %>% 
  separate("passive_methods", c("pt", "is", "ll", "sw", "as", "br", "tr"), remove = T)
separ[, c(7:13)]

# Exercicio 23 ------------------------------------------------------------

rem_na <- da %>% drop_na("year_start")
rem_na

# Exercicio 24 ------------------------------------------------------------
da_seleção <- da %>% 
  select(contains("method"))
da_seleção

# Exercicio 25 ------------------------------------------------------------
ggplot(da, aes(species_number)) + geom_histogram()

# Exercicio 26 ------------------------------------------------------------
da <- da %>% mutate(alt_log = log10(as.numeric(altitude)), 
                              tem_log = log10(as.numeric(temperature)), 
                              pre_log = log10(as.numeric(precipitation)))
# Exercicio 27 ------------------------------------------------------------
reordena <- da %>% arrange(desc(altitude))
reordena

# Exercicio 28 ------------------------------------------------------------
filtro <- da %>% filter(altitude > 1000, temperature < 15, precipitation > 1000 & precipitation <= 1500)
filtro

# Exercicio 29 ------------------------------------------------------------
aleat <- da %>% filter(species_number > 15) %>% sample_n(200)
aleat

# Exercicio 30 ------------------------------------------------------------

descrt <- da %>% drop_na() %>%  
  select(species_number, altitude, temperature, precipitation) %>% 
  map(range)
  
# Exercicio 31 ------------------------------------------------------------
 
### Uma opção é fazer a leitura dos dados com a função read_csv2() por que esta separa as colunas
### por tabulação e não de acordo com a separação por virgula, lendo a virgula da planilha como separador
### decimal, trocando a mesma para ponto na hora da leitura a manipulação.

# Exercicio 32 ------------------------------------------------------------
jpeg("altnum.jpeg")
g1 <- ggplot(da, aes(species_number, altitude)) + geom_point()
dev.off()
jpeg("tempnum.jpeg")
g2 <- ggplot(da, aes(species_number, temperature)) + geom_point()
dev.off()
jpeg("percnum.jpeg")
g3 <- ggplot(da, aes(species_number, precipitation)) + geom_point()
dev.off()

library(gridExtra)

grid.arrange(g1, g2, g3, nrow = 3)

### Existe uma relação entre as três variáveis e o número de espécies, de acordo com os gráficos, o maior número de espécies
### desta região costumam se agrupar em baixas/médias altitudes, evitanto extremos de temperatura e regiões em que a 
### precipitação também é mais baixa que a média, indicando que possuem estreita amplitude nas variações ambientais.

# Exercicio 33 ------------------------------------------------------------
nn <- table(da$state_abbreviation)
nn <- as.data.frame(nn)
jpeg("state.jpeg")
ggplot(da, aes(species_number)) + geom_histogram() + facet_wrap(as.factor(da$state_abbreviation)) + coord_flip()
dev.off()

# Exercicio 34 ------------------------------------------------------------
library(ggpubr)
ggboxplot(data = da, 
          x ="state_abbreviation", 
          y = "species_number",
add = "jitter", 
shape = "state_abbreviation",
fill = "state_abbreviation",
color = "black",
palette = "dark2",
xlab = "Número de espécies",
ylab = "Frequência absoluta",
legend = "none")
ggsave("exercicio_ggpubr.tiff")

# Exercicio 35 ------------------------------------------------------------
ggbarplot(da,
          x = "coordinate_precision",
          y = "species_number",
          color = "Black",
          lab.pos = "in", 
          lab.col = "white",
          ylim = c(0,10000))
ggsave("gps_precision.tiff")
