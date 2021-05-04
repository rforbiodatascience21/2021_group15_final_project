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
# Create factor variables where it is needed
kclust_data <- factor_columns(pbc_data_aug)

# We will use the  kmeans() function, which only takes numeric columns
# Change non numeric columns to numeric
kclust_data <- kclust_data %>% 
  mutate(sex = case_when(sex == "female" ~ 0,
                         sex == "male" ~ 1)) %>% 
  mutate(spiders = case_when(spiders == "absent" ~ 0,
                             spiders == "present" ~ 1)) %>% 
  mutate(hepatom = case_when(hepatom == "absent" ~ 0,
                             hepatom == "present" ~ 1)) %>% 
  mutate(ascites = case_when(ascites == "absent" ~ 0,
                             ascites == "present" ~ 1)) %>% 
  mutate(drug = case_when(drug == "placebo" ~ 0,
                          drug == "D-penicillamine" ~ 1)) %>% 
  mutate(mayo.risk.level = case_when(mayo.risk.level == "low risk" ~ 0,
                                     mayo.risk.level == "medium risk" ~ 1,
                                     mayo.risk.level == "high risk" ~ 2))

# Remove the stage column, since this is the one we are interested in. 
kclust_data <- 
  kclust_data %>% 
  select(-stage)


# K clustering with 4 clusters --------------------------------------------
kclust <- kmeans(kclust_data, centers = 4)
kclust

# The output is a list of vectors. 
# The cluster contains information about each point.
# centers, withinss and size contain information about each cluster.
# totss, tot.withinss, betweenss and iter contain information about the full clustering. 


# Plot only for 4 clusters ------------------------------------------------
kclusts <- 
  tibble(k = 4) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

#We can turn these into separate data sets 
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

# Plot the original points using the data from augment(), 
# with each point colored according to the predicted cluster.

p1 <- 
  ggplot(assignments, aes(x = mayo.risk, y = copper)) +
  geom_point(aes(color = .cluster), alpha = 0.8)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


# Plot 1 to 4 clusters -------------------------------------------------------

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

p1 <- 
  ggplot(assignments, aes(x = mayo.risk, y = albumin)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

#We can then add centers of the cluster using the data from tidy():
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


# Variance ----------------------------------------------------------------

# Plot of the the total within sum of squares and the number of clusters

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
