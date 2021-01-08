#Load Libraries
library(tidyverse)
library(readxl)
library(h2o)
library(rsample)
library(recipes)


#1 Load the training & test dataset
product_backorders_tbl <- read_csv("raw_data/product_backorders.txt")

set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(product_backorders_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)


#2 Specify the response and predictor variables
recipe_obj <- recipe(went_on_backorder ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)


#3 run AutoML specifying the stopping criterion
automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)


#4 View the leaderboard
automl_models_h2o@leaderboard


#5 Predicting using Leader Model

# Function that extracts H2O model name by position
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(1) %>% 
  h2o.getModel()

stacked_ensemble_h2o <- automl_models_h2o@leaderboard %>% 
                        extract_h2o_model_name_by_position(1) %>% 
                        h2o.getModel()

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)

predictions_tbl <- predictions %>% as_tibble()

predictions_tbl


#6 Save the leader model
stacked_ensemble_h2o %>% h2o.saveModel(path = "03_ml_aut_files/h20_models")
write_rds(automl_models_h2o,"automl_models_h2o.rds")
