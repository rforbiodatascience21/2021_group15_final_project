## Mie leger med clustering for status
#This section is based on this article: https://www.tidymodels.org/learn/statistics/k-means/ 

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidymodels)


# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# Fix factor ----------------------(SKAL ÆNDRES)----------------------
pbc_data_aug <- pbc_data_aug %>% 
  mutate(status = factor(status, levels = c(0,1))) %>% 
  mutate(stage = factor(stage, levels = c(1,2,3,4)))



# Exploratory plot to see if any cluster tendencies -----------------------
pbc_data_aug %>% 
  ggplot(mapping = aes(x = mayo_risk, y=albumin,color = status)) +
  geom_point(alpha=0.5)

# Data wrangling  ---------------------------------------------------------
# We will use the  kmeans() function. 
# It accepts a data frame only with all numeric columns.

# Remove the stage column, since this is the one we are interested in. 
kclust_data <- 
  pbc_data_aug %>% 
  select(-status)


# change non numeric columns to numeric
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
  mutate(mayo_risk_level = case_when(mayo_risk_level == "low risk" ~ 0,
                                     mayo_risk_level == "medium risk" ~ 1,
                                     mayo_risk_level == "high risk" ~ 2))


# K clustering with 4 clusters --------------------------------------------
kclust <- kmeans(kclust_data, centers = 2)
kclust

# The output is a list of vectors. 
# The cluster contains information about each point.
# centers, withinss and size contain information about each cluster.
# totss, tot.withinss, betweenss and iter contain information about the full clustering. 



# Plot only for 2 clusters ------------------------------------------------

kclusts <- 
  tibble(k = 2) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )
kclusts

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


# Plot the original points using the data from augment(), 
# with each point colored according to the predicted cluster.

p1 <- 
  ggplot(assignments, aes(x = mayo_risk, y = albumin)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

# Plot 1-2 clusters -------------------------------------------------------

kclusts <- 
  tibble(k = 1:2) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

kclusts


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
  ggplot(assignments, aes(x = mayo_risk, y = albumin)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1


# Variance ----------------------------------------------------------------

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


