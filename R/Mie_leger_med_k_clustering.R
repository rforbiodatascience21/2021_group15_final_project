## Mie leger med clustering

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidymodels)

#library(broom)
#library(purrr)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# fix factor
pbc_data_aug <- pbc_data_aug %>% 
  mutate(status = factor(status, levels = c(0,1))) %>% 
  mutate(stage = factor(stage, levels = c(1,2,3,4)))



# K-means
#This section is based on this article: https://www.tidymodels.org/learn/statistics/k-means/ 
pbc_data_aug %>% 
  ggplot(mapping = aes(x = mayo_risk, y=albumin,color = stage)) +
  geom_point(alpha=0.5)

#No clear clustering tendencies. 

#We’ll use the built-in kmeans() function, which accepts a data frame with all numeric columns as it’s primary argument.
kclust_data <- 
  pbc_data_aug %>% 
  select(-stage,-mayo_risk_level)


## change chr columns
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
                         drug == "D-penicillamine" ~ 1))




# k clustering with 4 clusters
kclust <- kmeans(kclust_data, centers = 4)
kclust

#The output is a list of vectors. 
#The cluster contains information about each point.
#centers, withinss and size contain information about each cluster.
#totss, tot.withinss, betweenss and iter contain information about the full clustering. 


#Which of these do we want to extract? 
#There is no right answer; each of them may be interesting to an analyst.
#Because they communicate entirely different information (not to mention there’s 
#no straightforward way to combine them), they are extracted by separate functions. 
#augment adds the point classifications to the original data set:

augment(kclust, kclust_data)


#The tidy() function summarizes on a per-cluster level:

tidy(kclust)


#And as it always does, the glance() function extracts a single-row summary:

glance(kclust)

# try to plot
# clusters <- kclust %>% 
#   unnest(cols=c(tidied))
# assignments <- kclust %>% 
#   unnest(cols=c(augmented))
# clusterings <- kclust %>% 
#   unnest(cols=c(glanced))


## Exploratory clustering
#Let’s say we want to explore the effect of different choices of k, from 1 to 9, on this clustering. First cluster the data 9 times, each using a different value of k, then create columns containing the tidied, glanced and augmented data:


kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(kclust_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kclust_data)
  )

kclusts


#We can turn these into three separate data sets each representing a different type of data: using tidy(), using augment(), and using glance(). Each of these goes into a separate data set as they represent different types of data.

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))


#Now we can plot the original points using the data from augment(), with each point colored according to the predicted cluster.

p1 <- 
  ggplot(assignments, aes(x = mayo_risk, y = albumin)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1


#Here we can see that there are still no really good clustering. 

#We can then add centers of the cluster using the data from tidy():

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


#The data from glance() fills a different but equally important purpose; it lets us view trends of some summary statistics across values of k. Of particular interest is the total within sum of squares, saved in the tot.withinss column.


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

#This represents the variance within the clusters. It decreases as k increases.

#If there is a bend, as in the article. The bend indicates that additional clusters beyond have little value. 