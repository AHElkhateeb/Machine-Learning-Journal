library(tidyverse)
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(rpart.plot)
library(workflows)
library(tidymodels)
library(broom.mixed)
library(modeldata)

#I. Build a model
#Gathering Data----
bike_features_tbl <- readRDS("Machine Learning Fundamentals data/bike_features_tbl.rds")
bike_features_tbl <- bike_features_tbl %>% 
  select(model:gender, `Rear Derailleur`, `Shift Lever`)

set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")
# Assign training and test data
train_tbl <- training(split_obj) 
test_tbl  <- testing(split_obj)
train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

#II. Create features with the recipes package
recipe_obj <- recipe(price ~ category_2 + frame_material + Rear_Derailleur + Shift_Lever, data = train_tbl) %>% 
  step_novel(all_predictors(), -all_numeric()) %>%
  step_dummy(Rear_Derailleur , Shift_Lever, one_hot = TRUE, preserve = TRUE) %>%
  step_nzv(all_predictors()) %>%
  prep()

train_transformed_tbl <- bake(recipe_obj, train_tbl) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
test_transformed_tbl  <- bake(recipe_obj, test_tbl)  %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))

#III. Bundle the model and recipe with the workflow package
model_linear_lm <- linear_reg(mode = "regression") %>%
  set_engine("lm")

bikes_wflow <- 
  workflow() %>% 
  add_model(model_linear_lm) %>% 
  add_recipe(recipe_obj)

#IV. Evaluate your model with the yardstick package
#Just use the function, that we have created in this session.

calc_metrics <- function(model, new_data = test_transformed_tbl) {
  model %>%
    predict(new_data = new_data) %>%
    bind_cols(new_data %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)
}

bikes_fit <- 
  bikes_wflow %>% 
  fit(data = train_transformed_tbl)

bikes_fit %>% calc_metrics(test_transformed_tbl)
