# Load only the tidymodels library
library(tidymodels)

# Data Sampling (rsample)
iris_split <- initial_split(iris, prop = 0.6)
iris_split

# train
iris_split %>%
  training() %>%
  glimpse()

# test
iris_split %>%
  testing() %>%
  glimpse()

# Pre-process interface
iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()