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

# Plots ---------------------------------------------------------------
box_plot <- kclust_data %>%
  ggplot(mapping = aes(x = sex, y = spiders, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()


ggplot(kclust_data) +
geom_bar(mapping = aes(x = stage, fill = spiders), position = "dodge")

# K clustering with 4 clusters --------------------------------------------
kclust <- kmeans(kclust_data, centers = 4)
kclust

# The output is a list of vectors. 
# The cluster contains information about each point.
# centers, withinss and size contain information about each cluster.
# totss, tot.withinss, betweenss and iter contain information about the full clustering. 



# Plot 1-4 clusters -------------------------------------------------------

kclusts <- 
  tibble(k = 1:4) %>%
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
  ggplot(assignments, aes(x = alk.phos, y = trig)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

###### Alk.phos - PÆN 

p1 <- 
  ggplot(assignments, aes(x = alk.phos, y = mayo.risk, color = status)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


###### Bili

p1 <- 
  ggplot(assignments, aes(x = bili, y = mayo.risk)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

###### fu.days

p1 <- 
  ggplot(assignments, aes(x = fu.days, y = mayo.risk)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

# Add centers of the cluster:
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


