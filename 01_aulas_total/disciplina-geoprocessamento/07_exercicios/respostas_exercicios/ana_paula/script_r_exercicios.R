------------------------------------------------------------------
  title: "Resolu??o exercicios da disciplina de geoprocessamento  para Etnobiologia e Conserva??o da Biodiversidade"
author: "Ana Paula G. Tavares"
------------------------------------------------------------------
  
###1
a <- 1
b <- a 
#' correcao
#' nao entendi, era apenas para criar o script


###2. cabe?alho criado
#' correcao
#' feito

###3.
help (ls)
(ls) 
function (name, pos = -1, envir = as.environment(pos), all.names = FALSE, 
          pattern, sorted = TRUE)
  ls(pattern="a")
#' correcao
rm(list = ls())


###4.
1:10
a<-2
a<-5
a<-3
(2 * 5) - (3 ^ 2)
#' correcao
abs((2 * 5) - (3 ^ 2))

###5. 
log10(10)
log(100)
log2(1000)

log10(10) + log(100) * log2(1000)
#' correcao
#' feito

###6.
10:10
fa_10=10:10
sqrt(fa_10)
#' correcao
fa_10 <- factorial(10)
fa_10

fa_10_rq <- sqrt(fa_10)
fa_10_rq

###7
s=400
t=3.5
s/t
#' correcao
s <- 400
s

t <- 3.5
t

v <- s / t
v 

###8
0:10
seq_10=0:10
+(seq_10) 
sum(seq_10)
seq_10_sum=55
#' correcao


###9. 
c <- seq(5, 50, 5)
c# 
seq_50=c
seq_50 <- rep(c,10)
seq_50_rep_times= rep(c,10)

###10.

i<-1
for(i in 8) {var(6,1:60):length(list)}

###11. 
sample(1, 1:12)
i <- 1
lista<-c
for(i in 25){  var <- sample (1, 1:12)
lista[i] <- var}

###12.
lo<-paste("local", 1:100, sep = "_")

###13.
lo <- paste("Local", seq(1:100), sep = "_")


###14.
a <- rep("cont",50)
b <- rep("trat",50)
tr <- factor(x = c(a,b), levels = c("cont","trat"))
t

###15.
v <- sample(0:10, 10000, replace = T)
ma <- matrix(data = v, nrow = 100, ncol = 100)
ma

###16.
library(tidyverse)
rpois(100,5)%>%
  sqrt()%>%
  exp()%>%
  log()%>%
  max()


###17.
sqrt(min(log10(exp(rnorm(100)))))


###18.
library(magrittr)
install.packages("magrittr")

1:10 %>% 
  sum %>% 
  divide_by(3) %>% 
  round(digits = 1)

###19
stwd ()
getwd()
dir ()
da <- read_csv("ATLANTIC_AMPHIBIANS_sites.csv")


###20.
tibble::glimpse (da)

###21.

