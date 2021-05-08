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
kclust_data <- pbc_data_aug %>% 
  mutate(sex = case_when(sex == "female" ~ 0,
                         sex == "male" ~ 1)) %>% 
  mutate_at(., 
            vars(spiders, hepatom, ascites), 
            list(~ case_when(. == "absent" ~ 0,
                             . == "present" ~ 1))) %>% 
  mutate(drug = case_when(drug == "placebo" ~ 0,
                          drug == "D-penicillamine" ~ 1)) %>% 
  mutate(mayo.risk.level = case_when(mayo.risk.level == "low risk" ~ 0,
                                     mayo.risk.level == "medium risk" ~ 1,
                                     mayo.risk.level == "high risk" ~ 2))

# Remove the stage column, since this is the one we are interested in. 
kclust_data <- 
  kclust_data %>% 
  select(-stage)

# Plot with 1-4 clusters --------------------------------------------------
# The clustering
kclusts <- 
  tibble(k = 1:4) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

#We can turn these into three separate data sets 
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Now we can plot the original points using the data from augment(), 
# with each point colored according to the predicted cluster.

plt_clust <- 
  ggplot(assignments, aes(x = alk.phos, y = mayo.risk)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)

#We can then add centers of the cluster using the data from tidy():
plt_clust_centers <- plt_clust + 
  geom_point(data = clusters, size = 10, shape = "x") +
  theme_classic() +
  labs(
    title = "K-means clustering",
    x = "Alkaline phosphatase (U/Liter)",
    y = "Mayo risk score",
    caption = "Data from https://hbiostat.org/data/",
    color = "Cluster"
  ) +
  theme(
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) 


# Clustering variance -----------------------------------------------------

kclusts <- 
  tibble(k = 1:6) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

#We can turn these into three separate data sets 
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Plot of the the total within sum of squares and the number of clusters
plt_clust_var <- ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    title = "K-means clustering",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) 

# Save plots --------------------------------------------------------------

plots <-
  c("plt_clust_centers",
    "plt_clust_var")

l <- mget(plots)

invisible(mapply(
  ggsave,
  file = paste0("results/", names(l), ".png"),
  plot = l,
  width = 12,
  height = 6
))

