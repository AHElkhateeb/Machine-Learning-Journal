# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

#Data Preparation & Feature Engineering
bike_features_tbl <- readRDS("Machine Learning Fundamentals data/bike_features_tbl.rds")
glimpse(bike_features_tbl)

bike_features_tbl <- bike_features_tbl %>% 
  select(model:url, `Rear Derailleur`, `Shift Lever`) %>% 
  mutate(
    `shimano dura-ace`        = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano dura-ace ") %>% as.numeric(),
    `shimano ultegra`         = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano ultegra ") %>% as.numeric(),
    `shimano 105`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano 105 ") %>% as.numeric(),
    `shimano tiagra`          = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano tiagra ") %>% as.numeric(),
    `Shimano sora`            = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano sora") %>% as.numeric(),
    `shimano deore`           = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano deore(?! xt)") %>% as.numeric(),
    `shimano slx`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano slx") %>% as.numeric(),
    `shimano grx`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano grx") %>% as.numeric(),
    `Shimano xt`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano deore xt |shimano xt ") %>% as.numeric(),
    `Shimano xtr`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano xtr") %>% as.numeric(),
    `Shimano saint`           = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano saint") %>% as.numeric(),
    `SRAM red`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram red") %>% as.numeric(),
    `SRAM force`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram force") %>% as.numeric(),
    `SRAM rival`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram rival") %>% as.numeric(),
    `SRAM apex`               = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram apex") %>% as.numeric(),
    `SRAM xx1`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram xx1") %>% as.numeric(),
    `SRAM x01`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram x01|sram xo1") %>% as.numeric(),
    `SRAM gx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram gx") %>% as.numeric(),
    `SRAM nx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram nx") %>% as.numeric(),
    `SRAM sx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram sx") %>% as.numeric(),
    `SRAM sx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram sx") %>% as.numeric(),
    `Campagnolo potenza`      = `Rear Derailleur` %>% str_to_lower() %>% str_detect("campagnolo potenza") %>% as.numeric(),
    `Campagnolo super record` = `Rear Derailleur` %>% str_to_lower() %>% str_detect("campagnolo super record") %>% as.numeric(),
    `shimano nexus`           = `Shift Lever`     %>% str_to_lower() %>% str_detect("shimano nexus") %>% as.numeric(),
    `shimano alfine`          = `Shift Lever`     %>% str_to_lower() %>% str_detect("shimano alfine") %>% as.numeric()
  ) %>% 
  # Remove original columns  
  select(-c(`Rear Derailleur`, `Shift Lever`)) %>% 
  # Set all NAs to 0
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 2.0 TRAINING & TEST SETS ----
bike_features_tbl <- bike_features_tbl %>% 
  
  mutate(id = row_number()) %>% 
  
  select(id, everything(), -url)

bike_features_tbl %>% distinct(category_2)

# run both following commands at the same time
set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")

# Check if testing contains all category_2 values
split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

# Assign training and test data
train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# We have to remove spaces and dashes from the column names
train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

# 3.0 LINEAR METHODS ----
# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----
?lm # from the stats package
?set_engine
?fit # then click Estimate model parameters and then fit at the bottom

model_01_linear_lm_simple <- linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(price ~ category_2 + frame_material, data = train_tbl)

?predict.model_fit 

model_01_linear_lm_simple %>%
  predict(new_data = test_tbl)

?metrics

model_01_linear_lm_simple %>%
  predict(new_data = test_tbl) %>%
  
  bind_cols(test_tbl %>% select(price)) %>%
  
  # Manual approach
  # mutate(residuals = price - .pred) %>% 
  # 
  # summarize(
  #   mae  = abs(residuals) %>% mean(),
  #   rmse = mean(residuals^2)^0.5
  # )
  
  yardstick::metrics(truth = price, estimate = .pred)

# 3.1.2 Feature Importance ----
View(model_01_linear_lm_simple) # You will see the coefficients in the element "fit"

# tidy() function is applicable for objects with class "lm"
model_01_linear_lm_simple$fit %>% class()

model_01_linear_lm_simple$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point(color = "#2dc6d6", size = 3) +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1, suffix = " €", prefix = "")),
                            size = 4, fill = "#272A36", color = "white") +
  scale_x_continuous(labels = scales::dollar_format(suffix = " €", prefix = "")) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 01: Simple lm Model")

# 3.1.3 Function to Calculate Metrics ----

# Code we used earlier
model_01_linear_lm_simple %>%
  predict(new_data = test_tbl) %>%
  
  bind_cols(test_tbl %>% select(price)) %>%
  yardstick::metrics(truth = price, estimate = .pred)

# Generalized into a function
calc_metrics <- function(model, new_data = test_tbl) {
  
  model %>%
    predict(new_data = new_data) %>%
    
    bind_cols(new_data %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)
  
}

model_01_linear_lm_simple %>% calc_metrics(test_tbl)

# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----
model_02_linear_lm_complex <- linear_reg("regression") %>%
  set_engine("lm") %>%
  
  # This is going to be different. Remove unnecessary columns.
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)

# 3.2.2 Feature importance ----
model_02_linear_lm_complex$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point(color = "#2dc6d6", size = 3) +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1, suffix = " €", prefix = "")),
                            size = 4, fill = "#272A36", color = "white") +
  scale_x_continuous(labels = scales::dollar_format(suffix = " €", prefix = "")) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 02: Complex lm Model")

# 3.3 PENALIZED REGRESSION ----

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

model_03_linear_glmnet <- linear_reg(mode    = "regression", 
                                     penalty = 10, 
                                     mixture = 0.1) %>%
  set_engine("glmnet") %>%
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_03_linear_glmnet %>% calc_metrics(test_tbl)

# 3.3.2 Feature Importance ----
model_03_linear_glmnet$fit %>%
  broom::tidy() %>%
  filter(lambda >= 10 & lambda < 11) %>%
  
  # No p value here
  arrange(desc(abs(estimate))) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                            size = 3) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 03: GLMNET Model")
