# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)


# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# Wrangle data ------------------------------------------------------------

# We dont want to include the variables we calculated from other variables 
# as these columns cannot be considered independent
pbc_data_aug <- pbc_data_aug %>% 
  select(-c(
    edema.score,
    mayo.risk,
    mayo.risk.level))

# For the PCA all columns must be numeric
pbc_data_aug <- pbc_data_aug %>% 
  mutate(sex = case_when(sex == "female" ~ 0,
                         sex == "male" ~ 1)) %>% 
  mutate_at(vars(spiders, 
                 hepatom, 
                 ascites),
            list(~ case_when(. == "absent" ~ 0,
                             . == "present" ~ 1)))

# Nest the tibble to make the PCA more straight forward
pbc_data_nested <- pbc_data_aug %>% 
  nest(pbc_data = everything()) 


# PCA ---------------------------------------------------------------------

# Make PCA and add augmented and tidied columns to the tibble
# we don't include "drug" in the pca 
# as this is the variable we are investigating
pbc_data_pca <- pbc_data_nested %>%
  mutate(pca = map(pbc_data,
                   ~ prcomp(
                     .x %>% 
                       select(-drug), 
                     center = TRUE,
                     scale = TRUE
                   ))) %>%
  mutate(pca_aug = map2(pca,
                        pbc_data, 
                        ~ augment(.x, 
                                  data = .y))) %>%
  mutate(pca_tidy = map2(pca, 
                         pbc_data, 
                         ~ tidy(.x, 
                                data = .y, 
                                matrix = "pcs")))
# "pcs" is an argument to the tidy function
# that gives information about the eigenvalues


# Visualization -----------------------------------------------------------


# Plot variance explained -------------------------------------------------

# For this we use the pca_tidy column
# which contains the variance explained by each pc
plt_pca_bar <- pbc_data_pca %>%
  unnest(pca_tidy) %>%
  ggplot(mapping = aes(
    x = PC, 
    y = percent)) +
  geom_bar(stat = "identity",
           fill = "#00BFC4") +
  geom_line(mapping = aes(
    y = cumulative,
    color = "#F8766D"
  )) +
  geom_point(mapping = aes(
    y = cumulative,
    color = "#F8766D"
  )) +
  geom_hline(yintercept = 0.9,
             linetype = "dashed",
             color = "gray") +
  labs(y = "Variance explained",
       title = "Individual PC and cumulative variance explained") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    plot.title.position = "plot",
    legend.position = "none"
  )


# Plot PC1 vs. PC2 --------------------------------------------------------

# For this we use the pca_augment column
# which contains the PCA values for each observation
plt_pca_scatter <- pbc_data_pca %>%
  unnest(pca_aug) %>%
  ggplot(mapping = aes(
    x = .fittedPC1, 
    y = .fittedPC2, 
    color = drug)) +
  geom_point() +
  labs(title = "Scatter plot of PC1 vs. PC2",
       x = "PC1", 
       y = "PC2",
       caption = "Data from https://hbiostat.org/data/") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top",
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    ) +
  scale_color_discrete(name = "Drug",
                       labels = c("placebo" = "Placebo"))

# Combine plots
plt_pca <- plt_pca_bar | plt_pca_scatter


# Save plots --------------------------------------------------------------
ggsave(
  file = "results/plt_pca.png",
  plot = plt_pca,
  width = 12,
  height = 6
)





