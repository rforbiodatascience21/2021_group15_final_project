# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)


# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# We dont want to include the variables we calculated from other variables 
# as these columns cannot be considered independent
pbc_data_aug <- pbc_data_aug %>% 
  select(-c(
    edema.score,
    mayo.risk,
    mayo.risk.level))

# Factor status to use as outcome variable
pbc_data_aug <- pbc_data_aug %>% 
  mutate(drug = factor(drug, 
                         levels = c("placebo", 
                                    "D-penicillamine")))


pbc_data_aug <- pbc_data_aug %>% 
  mutate(sex = case_when(sex == "female" ~ 0,
                         sex == "male" ~ 1)) %>% 
  mutate_at(vars(spiders, 
                 hepatom, 
                 ascites),
            list(~ case_when(. == "absent" ~ 0,
                             . == "present" ~ 1)))

pbc_data_nested <- pbc_data_aug %>% 
  nest(pbc_data=everything()) 

pbc_data_pca <- pbc_data_nested %>% 
  mutate(pca = map(pbc_data, 
                   ~ prcomp(.x %>% select(-drug), 
                            center = TRUE, 
                            scale = TRUE))) %>% 
  mutate(pca_aug = map2(pca, pbc_data, ~augment(.x, data = .y))) %>% 
  mutate(pca_tidy = map2(pca, pbc_data, ~tidy(.x, data = .y, "pcs")))



# Plot variance explained -------------------------------------------------

# For this we use the pca_tidy column
# which contains the variance explained by each pc
plt_pca_bar <- pbc_data_pca %>%
  unnest(pca_tidy) %>%
  ggplot(mapping = aes(
    x = PC, 
    y = percent)) +
  geom_bar(stat = "identity") +
  labs(y = "Variance Explained") +
  theme_classic()

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
  labs(x = "PC1", y = "PC2")


# Save plots --------------------------------------------------------------

# Make list with plot names
plots <-
  c("plt_pca_bar",
    "plt_pca_scatter")

# Get the plots
l <- mget(plots)

# Save
invisible(mapply(
  ggsave,
  file = paste0("results/", 
                names(l), 
                ".png"),
  plot = l,
  width = 12,
  height = 6
))





