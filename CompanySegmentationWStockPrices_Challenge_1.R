library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)

# STOCK PRICES
sp_500_prices_tbl <- read_rds("Machine Learning Fundamentals data/sp_500_prices_tbl.rds")
sp_500_prices_tbl

# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("raw_data/sp_500_index_tbl.rds")
sp_500_index_tbl

# Apply your data transformation skills!
sp_500_daily_returns_tbl<- sp_500_prices_tbl%>%
  select(symbol,date,adjusted)%>%
  filter(lubridate::year(date)>=2018)%>%
  group_by(symbol)%>%
  mutate(lag=lag(adjusted,1))%>%
  drop_na(lag)%>%
  mutate(diff=adjusted-lag)%>%
  mutate(pct_return=diff/lag)%>%
  select(symbol,date,pct_return)
# Output: sp_500_daily_returns_tbl

sp_500_daily_returns_tbl <- read_rds("raw_data/sp_500_daily_returns_tbl.rds")

# Convert to User-Item Format
stock_date_matrix_tbl <- sp_500_daily_returns_tbl%>%
                         pivot_wider(names_from = date , values_from = pct_return, values_fill=0)
# Output: stock_date_matrix_tbl

stock_date_matrix_tbl <- read_rds("raw_data/stock_date_matrix_tbl.rds")

# Create kmeans_obj for 4 centers
kmeans_obj<-stock_date_matrix_tbl%>%
  select(-symbol)%>%
  kmeans(centers = 4,nstart = 20)

# Apply glance() to get the tot.withinss
broom::glance(kmeans_obj)

kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

# Use purrr to map
k_means_mapped_tbl<-tibble(centers=1:30)%>%
  mutate(k_means = map(centers, kmeans_mapper))%>%
  mutate(glance  = map(k_means,glance))
# Output: k_means_mapped_tbl 


# Visualize Scree Plot

k_means_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") +
  # Formatting
  labs(title = "Scree Plot")


k_means_mapped_tbl <- read_rds("raw_data/k_means_mapped_tbl.rds")

# Apply UMAP
# Store results as: umap_results 
umap_results<-stock_date_matrix_tbl%>%
  select(-symbol)%>%
  umap()


# Convert umap results to tibble with symbols
umap_results_tbl <- umap_results$layout %>%
  as_tibble() %>%
  set_names(c("x", "y")) %>%
  bind_cols(
    stock_date_matrix_tbl %>% select(symbol)
  )
# Output: umap_results_tbl

# Visualize UMAP results
umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point(alpha=0.5) + 
  theme_tq()+
  labs(title = "UMAP Projection")

k_means_mapped_tbl <- read_rds("raw_data/k_means_mapped_tbl.rds")
umap_results_tbl   <- read_rds("raw_data/umap_results_tbl.rds")

# Get the k_means_obj from the 10th center
k_means_obj <- k_means_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)
# Store as k_means_obj

# Use your dplyr & broom skills to combine the k_means_obj with the umap_results_tbl
umap_kmeans_results_tbl<-k_means_obj%>%
  augment(stock_date_matrix_tbl) %>%
  select(symbol, .cluster) %>%
  left_join(umap_results_tbl, by= "symbol")%>%
  left_join(sp_500_index_tbl %>% select(symbol, company, sector), by="symbol")
# Output: umap_kmeans_results_tbl


# Visualize the combined K-Means and UMAP results
umap_kmeans_results_tbl%>%
  ggplot(aes(V1, V2, color = .cluster)) +
  geom_point(alpha=0.5) +
  scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692", "#d62dc5", "#d62d30","#ced62d", "#60d62d", "#571341", "#d62dc5", "#6e5552"))    
