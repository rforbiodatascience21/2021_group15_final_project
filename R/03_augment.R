# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("writexl")

# Load data ---------------------------------------------------------------
pbc_data_clean <- read_csv("data/02_pbc_data_clean.csv")


# Mutate edema column -----------------------------------------------------
pbc_data_clean <- pbc_data_clean %>%
  mutate(diuretic = ifelse(edema == "edema despite diuretic therapy", 1, 0)) %>%
  mutate(edema = ifelse(edema == "no edema", 0, 1))


# Create end age variable -------------------------------------------------
pbc_data_clean <- pbc_data_clean %>% 
  mutate(end_age = floor(age + (fu.days/365.25)))

str(pbc_data_clean)
