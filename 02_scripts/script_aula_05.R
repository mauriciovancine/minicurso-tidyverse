#' ---
#' title: aula 04 - resultados de modelos e tidymodels
#' author: mauricio vancine
#' date: 2020-04-26
#' ---

# topicos -----------------------------------------------------------------  
# 5.1 Pacote broom
# 5.2 Funções tidying do broom
# 5.3 Aplicações
# 5.4 Função tidy
# 5.5 Função glance
# 5.6 Função augment
# 5.7 Pacote tidymodels

# data
dplyr::glimpse(iris)

# linear model
lmfit <- lm(Sepal.Length ~ Petal.Length, iris)
lmfit

# summary
summary(lmfit)

# plot
ggplot(data = iris) +
  aes(x = Petal.Length, y = Sepal.Length) +
  geom_point(size = 4, col = "gray30") +
  stat_smooth(method = "lm", size = 2) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))

# package
library(broom)

# componentes do modelo
broom::tidy(lmfit)

# informacoes sobre o modelo inteiro
broom::glance(lmfit)

# observacoes do modelo
broom::augment(lmfit, data = iris)

# tidymodels --------------------------------------------------------------
# Load only the tidymodels library
library(tidymodels)

# data
dplyr::glimpse(iris)

# Data Sampling (rsample)
iris_split <- rsample::initial_split(iris, prop = 0.6)
iris_split

# train
iris_split %>%
  rsample::training() %>%
  dplyr::glimpse()

# test
iris_split %>%
  rsample::testing() %>%
  dplyr::glimpse()

# Pre-process interface (recipes)
iris_recipe <- rsample::training(iris_split) %>%
  recipes::recipe(Species ~.) %>%
  recipes::step_corr(all_predictors()) %>%
  recipes::step_center(all_predictors(), -all_outcomes()) %>%
  recipes::step_scale(all_predictors(), -all_outcomes()) %>%
  recipes::prep()
iris_recipe

# Execute the pre-processing (recipes)
# test data
iris_testing <- iris_recipe %>%
  recipes::bake(rsample::testing(iris_split)) 
dplyr::glimpse(iris_testing)

# train data
iris_training <- recipes::juice(iris_recipe)
dplyr::glimpse(iris_training)

# Model Training (parsnip)
iris_rf <- parsnip::rand_forest(trees = 100, mode = "classification") %>%
  parsnip::set_engine("randomForest") %>%
  parsnip::fit(Species ~ ., data = iris_training)

# Predictions
stats::predict(iris_rf, iris_testing)

iris_rf %>%
  stats::predict(iris_testing) %>%
  dplyr::bind_cols(iris_testing) %>%
  dplyr::glimpse()

# Model Validation (yardstick)
iris_rf %>%
  stats::predict(iris_testing) %>%
  dplyr::bind_cols(iris_testing) %>%
  yardstick::metrics(truth = Species, estimate = .pred_class)

# Per classifier metrics
iris_probs <- iris_rf %>%
  stats::predict(iris_testing, type = "prob") %>%
  dplyr::bind_cols(iris_testing)
dplyr::glimpse(iris_probs)

iris_probs %>%
  yardstick::gain_curve(Species, .pred_setosa:.pred_virginica) %>%
  dplyr::glimpse()

iris_probs%>%
  yardstick::roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  ggplot2::autoplot()

predict(iris_rf, iris_testing, type = "prob") %>%
  dplyr::bind_cols(predict(iris_rf, iris_testing)) %>%
  dplyr::bind_cols(select(iris_testing, Species)) %>%
  dplyr::glimpse()

predict(iris_rf, iris_testing, type = "prob") %>%
  dplyr::bind_cols(predict(iris_rf, iris_testing)) %>%
  dplyr::bind_cols(select(iris_testing, Species)) %>%
  yardstick::metrics(Species, .pred_setosa:.pred_virginica, estimate = .pred_class)

# end ---------------------------------------------------------------------