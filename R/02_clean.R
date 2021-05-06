# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(gt)

# Load data ---------------------------------------------------------------
pbc_data <- read_csv("data/01_pbc_data.csv")


# Look at missing values --------------------------------------------------
na_table <- pbc_data %>%
  summarise_all(list(~ sum(is.na(.))))

# Remove non-trial subjects -----------------------------------------------
pbc_data <- pbc_data %>% 
  filter(drug != "not randomized")

# Impute missing values ---------------------------------------------------

# How many observations have missing values now?
na_table2 <- pbc_data %>%
  summarise_all(list(~ sum(is.na(.))))

# Mutate missing values by assigning the mean
pbc_data <- pbc_data %>%
  mutate_all(list(~ ifelse(is.na(.), mean(., na.rm = TRUE),.)))

# Round off variables -----------------------------------------------------

# Round down age to whole years
pbc_data <- pbc_data %>% 
  mutate(age = floor(age))

# Round off numeric columns
pbc_data <- pbc_data %>% 
  mutate_if(is.numeric, round, digits = 2)

# Write data --------------------------------------------------------------
pbc_data_clean <- pbc_data

write_csv(x = pbc_data_clean, file = "data/02_pbc_data_clean.csv")




