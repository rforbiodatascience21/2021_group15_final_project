#This section is based on this article: https://www.tidymodels.org/learn/statistics/k-means/ 

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidymodels)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# Source functions --------------------------------------------------------
source("R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------

# We will use the  kmeans() function, which only takes numeric columns
# Change non numeric columns to numeric
kclust_data <- factor_columns(pbc_data_aug)

# Check that all values in mayo risk are unique
test <- kclust_data %>% 
  select(mayo.risk) %>% 
  summarise(count(distinct(.)))

# Save stage and mayo risk in separate tibble for later use
stage_data <- kclust_data %>% 
  select(stage, mayo.risk) %>% 
  mutate(mayo.risk = scale(mayo.risk))

# Remove stage and standardize data
kclust_data <- kclust_data %>% 
  select(-stage) %>% 
  scale(.)

# K-means clustering ------------------------------------------------------

# Perform k-means clustering and use tidyverse operators to get summaries
kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(x = kclust_data, 
                            centers = .x,
                            nstart = 25)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

# Get the summary of each clustering
clusterings <- 
  kclusts %>%
  unnest(glanced)

# To find the optimal number of clusters
# we look at the totalt within sum of squares of each cluter
plt_clust_var <- ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# It looks like 5 might be a good number of clusters 
# Filter kclusts to only get the data from 5 clusters
five_clusts <- kclusts %>% 
  filter(k == 4)

five_clusts %>% 
  select(kclust) %>% 
  unnest()

# Get information about each cluster
clusters <- 
  five_clusts %>%
  unnest(tidied)

# Get assignment of each observation to a cluster
assignments <- 
  five_clusts %>% 
  unnest(augmented) %>% select(-k, -kclust, -tidied, -glanced)

assign_longer <-
  left_join(assignments, stage_data, by = "mayo.risk") %>%
  group_by(.cluster) %>%
  ungroup() %>%
  pivot_longer(!c(stage, .cluster),
               names_to = "attribute",
               values_to = "value") 
  
  
assign_longer %>% ggplot(aes(x = stage, y = value, fill = .cluster)) +
geom_col() +
coord_flip() +
facet_wrap(~attribute, drop=TRUE, scales = "free") +
theme(axis.text.y = element_blank())

# Now we can plot the original points using the data from augment(), 
# with each point colored according to the predicted cluster.

plt_clust <- 
  ggplot(assignments, aes(x = age, y = mayo.risk)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k) +
  geom_point(data = clusters, size = 4, shape = "x")



