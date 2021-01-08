#Code from previous challenge
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

#import leaderboard
automl_models_h2o <- read_rds("automl_models_h2o.rds")
h2o_leaderboard <- automl_models_h2o@leaderboard


#General functions
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

#New Theme
theme_new <- theme(
  legend.position  = "bottom",
  legend.key       = element_blank(),
  panel.background = element_rect(fill   = "transparent"),
  panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
  panel.grid.major = element_line(color = "grey", size = 0.333)
) 



#1 Leaderboard visualization

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  
  # Setup inputs
  # adjust input so that all formats are working
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    select(-c(aucpr, mean_per_class_error, rmse, mse)) %>% 
    mutate(model_type = str_extract(model_id, "[^_]+")) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", model_id) %>% as.factor())
  
  # Transformation
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      pivot_longer(cols = -c(model_id, model_type, rowname), 
                   names_to = "key", 
                   values_to = "value", 
                   names_transform = list(key = forcats::fct_inorder)
      )
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      pivot_longer(cols = -c(model_id, model_type, rowname), 
                   names_to = "key", 
                   values_to = "value", 
                   names_transform = list(key = forcats::fct_inorder)
      )
    
  } else {
    # If nothing is supplied
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }
  
  # Visualization
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")
  
  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), 
                                           hjust = "inward"))
  
  return(g)
  
}

plot_h2o_leaderboard(h2o_leaderboard, order_by = "auc", 
                     n_max = 15, size = 3, include_lbl = TRUE)



#2 Tune the deeplearning model with grid search

deeplearning_h2o <-automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(12) %>% 
  h2o.getModel()

deeplearning_h2o

h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))


deeplearning_grid_01 <- h2o.grid(
  
  # See help page for available algos
  algorithm = "deeplearning",
  
  # I just use the same as the object
  grid_id = "deeplearning_grid_01",
  
  # predictor and response variables
  x = x,
  y = y,
  
  # training and validation frame and crossfold validation
  training_frame   = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  
  # Hyperparamters: Use deeplearning_h2o@allparameters to see all
  hyper_params = list(
    # Use some combinations (the first one was the original)
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

deeplearning_grid_01




#3 Visualize the trade of between the precision and the recall and the optimal threshold
stacked_ensemble_h2o <- automl_models_h2o@leaderboard %>% extract_h2o_model_name_by_position(1) %>% h2o.getModel()

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble() 


performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  
  # Insert line where precision and recall are harmonically optimized
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  labs(title = "Precision vs Recall", y = "value") +
  theme_new











#Plots
# Performance Visualization ----  
library(cowplot)
library(glue)

# Inputs
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "logloss" 
size <- 0.5
max_models <- 4


leaderboard_tbl <- h2o_leaderboard %>%
  as_tibble() %>%
  slice(1:max_models)

newdata_tbl <- newdata %>%
  as_tibble()

# Selecting the first, if nothing is provided
order_by      <- tolower(order_by[[1]]) 

# Convert string stored in a variable to column name (symbol)
order_by_expr <- rlang::sym(order_by)

# 1. Model metrics

get_model_performance_metrics <- function(model_id, test_tbl) {
  
  model_h2o <- h2o.getModel(model_id)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    select(threshold, tpr, fpr, precision, recall)
  
}

model_metrics_tbl <- leaderboard_tbl %>%
  mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
  unnest(cols = metrics) %>%
  mutate(
    model_id = as_factor(model_id) %>% 
      # programmatically reorder factors depending on order_by
      fct_reorder(!! order_by_expr, 
                  .desc = ifelse(order_by == "auc", TRUE, FALSE)),
    auc      = auc %>% 
      round(3) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id)),
    logloss  = logloss %>% 
      round(4) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id))
  )


#4 ROC Plot

roc_plot <- model_metrics_tbl %>%
  ggplot(aes(fpr, tpr, color = model_id, linetype = !! order_by_expr)) +
  geom_line(size = size) +
  theme_new +
  labs(title = "ROC", x = "FPR", y = "TPR") +
  theme(legend.direction = "vertical") 

roc_plot


#5 Precision vs Recall Plot

precision_recall_plot <- model_metrics_tbl %>%
  ggplot(aes(recall, precision, color = model_id, linetype = !! order_by_expr)) +
  geom_line(size = size) +
  theme_new +
  labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
  theme(legend.position = "none") 

precision_recall_plot

# Gain / Lift

get_gain_lift <- function(model_id, test_tbl) {
  
  model_h2o <- h2o.getModel(model_id)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.gainsLift() %>%
    as.tibble() %>%
    select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
  
}

gain_lift_tbl <- leaderboard_tbl %>%
  mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
  unnest(cols = metrics) %>%
  mutate(
    model_id = as_factor(model_id) %>% 
      fct_reorder(!! order_by_expr, 
                  .desc = ifelse(order_by == "auc", TRUE, FALSE)),
    auc  = auc %>% 
      round(3) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id)),
    logloss = logloss %>% 
      round(4) %>% 
      as.character() %>% 
      as_factor() %>% 
      fct_reorder(as.numeric(model_id))
  ) %>%
  rename(
    gain = cumulative_capture_rate,
    lift = cumulative_lift
  ) 

#6 Gain Plot

gain_plot <- gain_lift_tbl %>%
  ggplot(aes(cumulative_data_fraction, gain, 
             color = model_id, linetype = !! order_by_expr)) +
  geom_line(size = size,) +
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
               color = "red", size = size, linetype = "dotted") +
  theme_new +
  expand_limits(x = c(0, 1), y = c(0, 1)) +
  labs(title = "Gain",
       x = "Cumulative Data Fraction", y = "Gain") +
  theme(legend.position = "none")

gain_plot


#7 Lift Plot

lift_plot <- gain_lift_tbl %>%
  ggplot(aes(cumulative_data_fraction, lift, 
             color = model_id, linetype = !! order_by_expr)) +
  geom_line(size = size) +
  geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
               color = "red", size = size, linetype = "dotted") +
  theme_new +
  expand_limits(x = c(0, 1), y = c(0, 1)) +
  labs(title = "Lift",
       x = "Cumulative Data Fraction", y = "Lift") +
  theme(legend.position = "none") 

lift_plot


#8 Dashboard with cowplot

# cowplot::get_legend extracts a legend from a ggplot object
p_legend <- get_legend(roc_plot)
# Remove legend from roc_plot
roc_plot <- roc_plot + theme(legend.position = "none")

# cowplot::plt_grid() combines multiple ggplots into a single cowplot object
p <- cowplot::plot_grid(roc_plot, precision_recall_plot, gain_plot, lift_plot, ncol = 2)

# cowplot::ggdraw() sets up a drawing layer
p_title <- ggdraw() + 
  
  # cowplot::draw_label() draws text on a ggdraw layer / ggplot object
  draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
             color = "#2C3E50")

p_subtitle <- ggdraw() + 
  draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
             color = "#2C3E50")

# Combine everything
cow_plot_dashboard <- plot_grid(p_title, p_subtitle, p, p_legend, 
                                
                                # Adjust the relative spacing, so that the legends always fits
                                ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))

cow_plot_dashboard
