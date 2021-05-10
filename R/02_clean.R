# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gt) # generate display tables


# Load data ---------------------------------------------------------------
pbc_data <- read_csv("data/01_pbc_data.csv")


# Look at missing values --------------------------------------------------
na_table1 <- pbc_data %>%
  summarise_all(list(~ sum(is.na(.))))


# Remove non-trial subjects -----------------------------------------------
pbc_data <- pbc_data %>% 
  filter(drug != "not randomized")


# Missing values after removal of non-trial subjects ----------------------

# How many observations have missing values now?
na_table2 <- pbc_data %>%
  summarise_all(list(~ sum(is.na(.))))


# Table of missing values in data -----------------------------------------

# Create a long table of the  missing values
na_table1_long <- na_table1 %>%
  pivot_longer(all_of(colnames(.)),
               names_to = "Variable",
               values_to = "NA in raw data")

na_table2_long <- 
  na_table2 %>%  pivot_longer(all_of(colnames(.)),
                              names_to = "Variable", 
                              values_to = "NA in clean data")

# Joing the two tibbles
na_values <- full_join(na_table1_long, 
                       na_table2_long, 
                       by = "Variable")


# Impute missing values ---------------------------------------------------

# Mutate missing values by assigning the mean
pbc_data <- pbc_data %>%
  mutate_all(list( ~ ifelse(is.na(.), 
                            mean(., na.rm = TRUE), 
                            .)))


# Round off variables -----------------------------------------------------

# Round down age to whole years
pbc_data <- pbc_data %>%
  mutate(age = floor(age))

# Round off numeric columns
pbc_data <- pbc_data %>%
  mutate_if(is.numeric, 
            round, 
            digits = 2)


# Write data --------------------------------------------------------------
pbc_data_clean <- pbc_data

write_csv(x = pbc_data_clean, 
          file = "data/02_pbc_data_clean.csv")

# Create a table with NA values
na_table <- na_values %>% 
  gt() %>% 
  tab_header(
    title = md("Table of missing values in the data"))

# Save in results
na_table %>% 
  gtsave(filename = "results/na_table.png")



