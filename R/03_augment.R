# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------
pbc_data_clean <- read_csv("data/02_pbc_data_clean.csv")

# Calculate Mayo Risk score -----------------------------------------------

# create edema score column
pbc_data_clean <- pbc_data_clean %>% 
  mutate(edema_score = case_when(edema == "no edema" ~ 0,
                                 edema == "edema, no diuretic therapy" ~ 0.5,
                                 edema == "edema despite diuretic therapy" ~ 1)) %>% 
  relocate(edema_score, .after = edema)

pbc_data_clean <- pbc_data_clean %>% 
  mutate(mayo_risk = 
           0.04 * age + 
           10.87 * log(bili) - 
           22.53 * log(albumin) +
           12.38 * log(protime) +
           10.86 * edema_score)

# convert to risk level
pbc_data_clean <- pbc_data_clean %>%
  mutate(
    mayo_risk_level = case_when(
      mayo_risk < 8.5 ~ "low risk",
      8.5 <= mayo_risk &
        mayo_risk <= 10 ~ "medium risk",
      mayo_risk > 10 ~ "high risk"
    )
  )

pbc_data_clean <- pbc_data_clean %>%
  mutate(mayo_risk_level = factor(mayo_risk_level,
                                  levels =  c("low risk",
                                              "medium risk",
                                              "high risk")))



# Mutate edema column -----------------------------------------------------
pbc_data_clean <- pbc_data_clean %>%
  mutate(diuretic = ifelse(edema == "edema despite diuretic therapy", 1, 0)) %>%
  mutate(edema = ifelse(edema == "no edema", 0, 1)) %>% 
  relocate(diuretic, .after = edema)


# Create end age variable -------------------------------------------------
pbc_data_clean <- pbc_data_clean %>% 
  mutate(end_age = floor(age + (fu.days/365.25))) %>% 
  relocate(end_age, .after = age)


# Write data --------------------------------------------------------------
write_csv(x = pbc_data_clean, file = "data/03_pbc_data_aug.csv")
