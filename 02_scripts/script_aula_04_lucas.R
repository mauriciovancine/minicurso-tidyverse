  #' ---
#' title: aula 04 - visualizacao de dados 
#' author: mauricio vancine
#' date: 2020-04-26
#' ---

# topicos -----------------------------------------------------------------  
# 4.1 Principais pacotes para gráficos 
# 4.2 principais livros e sites 
# 4.3 tipos de dados (variaveis = colunas)
# 4.4 principais tipos de graficos
# 4.5 graficos no r (pacotes graphics, ggplot2 e ggpubr)
# 4.6 histograma (histogram)
# 4.7 grafico de setores (pie chart e danut plot)
# 4.8 grafico de barras (bar plot)
# 4.9 grafico de caixa (box plot)
# 4.10 grafico de dispersao (scatter plot)

# 4.1 Principais pacotes para gráficos ------------------------------------
# package
library(tidyverse)

# directory
setwd("/home/lucas/Documentos/data/github/minicurso-tidyverse/03_dados")

# importar
da <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv")
da

# verificar
glimpse(da)

# graphics
plot(species_number ~ effort_months, data = da)

# ggplot2
library(ggplot2)
ggplot(data = da) + aes(effort_months, species_number) + geom_point()

# ggpubr
library(ggpubr)
ggscatter(da, x = "effort_months", y = "species_number")

# 4.5 histograma (histogram) ----------------------------------------------
# graphics

# histogram  --> representa dados de 1 coluna apenas - distribuição de frequencia e densidade dos dados contínuos
hist(da$species_number)

# color
hist(da$species_number,
     col = "gray50",
     border = "gray")

# tible
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti")

# labs
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr")

# breacks
hist(da$species_number,
     col = "gray50",
     border = "gray",
     main = "Ti",
     xlab = "Sp",
     ylab = "Fr",
     br = 50)

# size
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

# sensity
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

# diretorio para salvar
setwd("/home/lucas/Documentos/data/github/minicurso-tidyverse/03_dados")
# cria um arquivo vazio
png("meu_primeiro_histograma.png", wi = 15, he = 15, un = "cm", 
     res = 300)
# plot
par(mar = c(6, 6, 2, 2),  ##aqui é para não cortar o grafico. São margens
    mfrow = c(1,2))  
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

# aes - estética... 
ggplot(data = da) +
  aes(x = species_number)

# geom
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram()

# density
ggplot(data = da) +
  aes(x = species_number) +
  geom_density()

# change
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10)

# theme
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, alpha = .5) +
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# density
ggplot(data = da) +
  aes(x = species_number) +
  geom_density(color = "black", fill = "forest green", alpha = .5) + 
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# facet
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_wrap(~ record, ncol = 2, scale = "free_y") +
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# facet
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_wrap(~ record, ncol = 2) +
  labs(x = "Número de Espécies", y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# facet
ggplot(data = da) +
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10, 
                 alpha = .5) +
  facet_grid(record ~ .) +
  labs(x = "Número de Espécies",
       y = "Frequência") +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# theme_
ggplot(data = da) +
  aes(x = species_number) +
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
  aes(x = species_number) +
  geom_histogram(color = "black", fill = "forest green", bins = 10,
                 alpha = .5) +
  facet_grid(record ~ .) +
  labs(x = "Número de Espécies",
          y = "Frequência") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
ggsave("histogram_ggplot2.png", whwi = 20, he = 15, un = "cm", dpi = 300)

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
gghistogram(data = da,
            x = "species_number",
            add = "median",
            fill = "steelblue",
            rug = TRUE,
            add_density = TRUE,
            xlab = "Número de espécies",
            ylab = "Frequeência absoluta")
ggsave("histogram_ggpubr.png", wi = 20, he = 15, un = "cm", dpi = 300)


# 4.6 grafico de setores (pie chart) --------------------------------------
# graphics
# frequence table
ta <- table(da$record)
ta <- round(ta/sum(ta) * 100, 2)
ta

# pie chart
pie(ta,
    labels = paste(ta, "%"),
    main = "Tipos de amostragens",
    col = c("steelblue", "brown"))
legend("topright", c("Abundância", "Composição"), 
       cex = 0.8, fill = c("steelblue", "brown"))

# ggplot2
# frequence table as data frame
ta_por <- ta %>%
  as.data.frame %>% 
  mutate(Amostragem = c("Abundância", "Composição"),
         porc = paste0(Freq, "%"))
ta_por

# pie chart  ## esse é toppp
ggplot(ta_por) +
  aes(x = "", y = Freq, fill = Amostragem) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = porc), color = "white", 
            position = position_stack(vjust = 0.5), size = 8) +
  scale_fill_manual(values = c(c("#0073C2FF", "#EFC000FF"))) +
  theme_void()

# ggpubr
# pie chart
ggpie(ta_por,
      "Freq", 
      label = "porc",
      lab.pos = "in", 
      lab.font = c(5, "white"),
      fill = "Amostragem", 
      color = "white",
      palette = c("#00AFBB", "#FC4E07"))

# 4.6 grafico de setores (donut chart) ------------------------------------
# ggplot2
# donut
ggplot(ta_por) +
  aes(x = 2, y = Freq, fill = Amostragem) +
  geom_bar(stat = "identity", color = "white") +
  xlim(0, 2.5) +
  coord_polar(theta ="y", start = 0) +
  geom_text(aes(label = porc), color = "white", 
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c(c("#0073C2FF", "#EFC000FF"))) +
  theme_void() +
  theme(legend.position = c(.5, .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))

# ggpubr
# pie
ggdonutchart(ta_por,
             "Freq", 
             label = "porc",
             lab.pos = "in", 
             lab.font = c(4, "white"),
             fill = "Amostragem", 
             color = "white",
             palette = c("#00AFBB", "#FC4E07"))


# 4.7 grafico de barras (bar plot) ----------------------------------------
# frequency table
ta <- table(da$record)
names(ta) <- c("Abundância", "Composição")
ta

# graphics
barplot(ta, 
        col = c("blue", "forest green"),
        main = "Frequência de tipos de amostragens",
        xlab = "Tipo de registro",
        ylab = "Frequência",
        cex.main = 1.5,
        cex.lab = 1.3,
        cex.axis = 1.2)

# frequency table as data frame
ta_por <- ta %>% 
  as.data.frame
colnames(ta_por) <- c("record", "freq")
ta_por

# ggplot2
ggplot(data = ta_por) +
  aes(x = record, y = freq) +
  geom_bar(fill = c("blue", "forest green"), stat = "identity") +
  geom_text(aes(x = record, y = freq, label = freq), size = 8, color = "white", vjust = 2) +
  labs(x = "Tipo de registro",
       y = "Frequência") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# ggpubr
ggbarplot(ta_por,
          x = "record",
          y = "freq", 
          fill = "record", 
          color = "record",
          palette = c("#00AFBB", "#FC4E07"),
          label = TRUE, 
          lab.pos = "in", 
          lab.col = "white",
          lab.size = 8,
          xlab = "Tipo de registro",
          ylab = "Frequeência absoluta",
          legend = "none")


# 4.8 grafico de caixa (box plot) -----------------------------------------
# graphics
boxplot(species_number ~ as.factor(record),  # y em funsao de x... x precisa ser fator
        data = da,
        col = "gray",
        border = "black",
        main = "Espécies por amostragens",
        xlab = "Tipo de registro",
        ylab = "Número de Espécies",
        cex.main = 1.5,
        cex.lab = 1.3,
        cex.axis = 1.2)

# ggplot2
# boxplot
ggplot(data = da) +
  aes(x = record, y = species_number) +
  geom_boxplot(fill = c("blue", "forest green"), color = "black") +
  labs(x = "Tipo de registro",
       y = "Número de espécies") +
  theme_classic() + 
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# boxplot with jitter
ggplot(data = da) +
  aes(x = record, y = species_number) +
  geom_boxplot(fill = c("blue", "forest green"), color = "black") +
  geom_jitter(width = .3, alpha = .4) +
  labs(x = "Tipo de registro",
       y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# violin
ggplot(data = da) +
  aes(x = record, y = species_number) +
  geom_violin(aes(fill = record), color = "black") +
  scale_fill_manual(values = c("blue", "forest green")) +
  geom_jitter(width = .3, alpha = .3) +
  labs(x = "Tipo de registro",
       y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# ggpubr
# boxplot
ggboxplot(data = da, 
          x = "record", 
          y = "species_number",
          xlab = "Número de espécies",
          ylab = "Frequeência absoluta")

# boxplot
ggboxplot(data = da, 
          x = "record", 
          y = "species_number",
          fill = "record",
          palette = c("#00AFBB", "#FC4E07"),
          xlab = "Número de espécies",
          ylab = "Frequeência absoluta",
          legend = "none")

# boxplot with jitter
ggboxplot(data = da, 
          x = "record", 
          y = "species_number",
          add = "jitter", 
          shape = "record",
          fill = "record",
          color = "black",
          palette = c("#00AFBB", "#FC4E07"),
          xlab = "Número de espécies",
          ylab = "Frequeência absoluta",
          legend = "none")

# violin
ggviolin(data = da, 
         x = "record", 
         y = "species_number",
         add = "jitter", 
         shape = "record",
         fill = "record",
         color = "black",
         palette = c("#00AFBB", "#FC4E07"),
         xlab = "Número de espécies",
         ylab = "Frequeência absoluta",
         legend = "none")


# 4.9 grafico de dispersao (scatter plot) ---------------------------------
# graphics    x e y numericos
plot(species_number ~ effort_months,
     data = da,
     pch = 20,
     xlab = "Esforço amostral",
     ylab = "Número de espécies",
     cex.lab = 1.5,
     cex.axis = 1.3,
     bty = "l")

# ggplot2
ggplot(data = da) +
  aes(x = effort_months, y = species_number) +
  geom_point(colour = "black", fill = "forest green", size = 5, 
             alpha = .5, pch = 21) +
  labs(x = "Esforço amostral", y = "Número de espécies") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

#ggplot2
ggplot(data = da) +
  aes(x = effort_months, y = species_number) +
  geom_point(colour = "black", fill = "forest green", size = 5, 
             alpha = .5, pch = 21) +
  labs(x = "Esforço amostral", y = "Número de espécies") +
  theme_classic()+
  stat_smooth(method = "lm", color = "red", size = 2) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# ggpubr
ggscatter(data = da,
          x = "effort_months", 
          y = "species_number",
          color = "black",
          fill = "forestgreen",
          shape = 21,
          size = 5,
          xlab = "Esforço amostral", 
          ylab = "Número de espécies")


# bonus -------------------------------------------------------------------

# package
library(datasauRus)

# data + plot
datasaurus_dozen %>% 
  dplyr::filter(dataset == "dino") %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(colour = "black", fill = "black", 
             size = 5, alpha = .75, pch = 21) +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# all plot
datasaurus_dozen %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(colour = "black", fill = "black", 
             size = 2, alpha = .75, pch = 21) +
  facet_wrap(~dataset) +
  theme_bw()

# pairs -------------------------------------------------------------------
# selecionar as colunas
da_sel <- da %>% 
  select(temperature, precipitation, altitude)
da_sel

# pairs plot
pairs(da_sel,
      pch = 20,
      upper.panel = NULL)

# package
library(GGally)

# ggpairs
ggpairs(data = da_sel) +
  theme_bw()

# psych
# package
library(psych)

# pairs
pairs.panels(da_sel, 
             method = "spearman",
             pch = 20, 
             ellipses = FALSE, 
             density = FALSE, 
             stars = TRUE, 
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)
# end ---------------------------------------------------------------------