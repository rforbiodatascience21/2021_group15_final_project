# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------
pbc_data <- read_csv("data/01_pbc_data.csv")


# Remove non-trial subjects -----------------------------------------------
pbc_data <- pbc_data %>% 
  filter(drug != "not randomized")


# Impute missing values ---------------------------------------------------

# how many observations have missing values?
pbc_data %>%
  select_if(is.numeric) %>% summarise_all(list(~ sum(is.na(.))))

# mutate missing values
pbc_data <- pbc_data %>%
  mutate_all(list(~ ifelse(is.na(.), mean(., na.rm = TRUE),.)))


# Round off variables -----------------------------------------------------

# round down age to whole years
#pbc_data <- pbc_data %>% 
#  mutate(age = floor(age))

# round off numeric columns
pbc_data <- pbc_data %>% 
  mutate_if(is.numeric, round, digits = 2)

# Write data --------------------------------------------------------------
pbc_data_clean <- pbc_data

write_csv(x = pbc_data_clean, file = "data/02_pbc_data_clean.csv")




