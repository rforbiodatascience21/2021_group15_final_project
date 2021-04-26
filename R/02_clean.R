# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("writexl")

# Load data ---------------------------------------------------------------
pbc_data <- read_xlsx("data/_raw/pbc.xlsx")


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
pbc_data <- pbc_data %>% 
  mutate(age = floor(age))

# round off numeric columns
pbc_data <- pbc_data %>% 
  mutate_if(is.numeric, round, digits = 2)


# Convert to factors ------------------------------------------------------
pbc_data <- pbc_data %>% 
  mutate(sex = factor(sex, levels = c("male", "female")))

pbc_data <- pbc_data %>%
  mutate_at(., 
            vars(spiders, hepatom, ascites), 
            list(~ factor(., levels = c("absent", "present"))))

str(pbc_data)

# Write data --------------------------------------------------------------
pbc_data_clean <- pbc_data

write_csv(x = pbc_data_clean, file = "data/02_pbc_data_clean.csv")




