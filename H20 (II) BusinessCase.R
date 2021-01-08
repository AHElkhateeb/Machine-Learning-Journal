# Load data
library(tidyverse)
library(readxl)

employee_attrition_tbl <- read_csv("raw_data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt")
definitions_raw_tbl    <- read_excel("raw_data/data_definitions.xlsx", sheet = 1, col_names = FALSE)
View(definitions_raw_tbl)

# Data preparation ----
# Human readable

definitions_tbl <- definitions_raw_tbl %>% 
  fill(...1, .direction = "down") %>%
  filter(!is.na(...2)) %>%
  separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = ...1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 
definitions_tbl

# DATA PREPARATION ----
# Human readable ----

definitions_list <- definitions_tbl %>% 
  
  # Mapping over lists
  
  # Split into multiple tibbles
  split(.$column_name) %>%
  # Remove column_name
  map(~ select(., -column_name)) %>%
  # Convert to factors because they are ordered an we want to maintain that order
  map(~ mutate(., value = as_factor(value))) 

# definitions_list[[1]]
definitions_list[["Education"]]

# Rename columns
for (i in seq_along(definitions_list)) {
  list_name <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
}

definitions_list[["Education"]]

data_merged_tbl <- list(HR_Data = employee_attrition_tbl) %>%
  
  # Join everything
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  
  # Remove unnecessary columns
  select(-one_of(names(definitions_list))) %>%
  
  # Format the "_value"
  set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
  
  # Resort
  select(sort(names(.))) 

# Return only unique values of BusinessTravel
data_merged_tbl %>% 
  distinct(BusinessTravel)

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%        
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                    "Travel_Rarely", 
                                                    "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                   "Married", 
                                                   "Divorced")
  )

data_processed_tbl %>% 
  select_if(is.factor) %>% 
  map(levels)

#processing pipeline function
process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}
process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl) %>% 
  glimpse()

# DATA PREPARATION ----
# Machine readable ----

# libraries
library(rsample)
library(recipes)

# Processing pipeline
# If we had stored our script in an external file
#source("00_scripts/data_processing_pipeline.R")

# If we had our raw data already split into train and test data
#train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
#test_redable_tbl   <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)



# Plot Faceted Histgoram function

# To create a function and test it, we can assign our data temporarily to data
data <- train_readable_tbl 

plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = "#2dc6d6", color = "white", 
                            ncol = 5, scale = "free") {
  
  data_factored <- data %>%
    
    # Convert input to make the function fail safe 
    # (if other content might be provided)
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    
    # Data must be in long format to make facets
    pivot_longer(cols = everything(),
                 names_to = "key",
                 values_to = "value",
                 # set key = factor() to keep the order
                 names_transform = list(key = forcats::fct_inorder)) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale)
  
  return(g)
  
}

# Example calls
train_readable_tbl %>% plot_hist_facet()
train_readable_tbl %>% plot_hist_facet(fct_rev = T)

# Bring attirtion to the top (alt.: select(Attrition, everything()))
train_readable_tbl %>% 
  relocate(Attrition) %>% 
  plot_hist_facet()


# Data Preprocessing With Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj %>% 
  prep()
