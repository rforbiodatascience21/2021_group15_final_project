# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(broom)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# Source functions --------------------------------------------------------
source("R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
#pbc_data_aug <- factor_columns(pbc_data_aug)

# Linear regression model -------------------------------------------------

# first we scale the data
pbc_data_scaled <- pbc_data_aug %>% 
  mutate_if(is.numeric, scale) # scale subtracts the mean and divide by the sd 

# remove variables that are not needed
pbc_data_scaled <- pbc_data_scaled %>% 
  select(-age, -albumin, - bili, -protime, -end.age, -edema.score)

# make linear model
lm <- lm(status ~ ., data = pbc_data_scaled)

lm_tidy <- tidy(lm, conf.int = TRUE)

# select p-values < 0.05
lm_tidy %>% 
  filter(p.value < 0.05)
