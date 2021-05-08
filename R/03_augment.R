# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------
pbc_data_clean <- read_csv("data/02_pbc_data_clean.csv")

# Calculate Mayo Risk score -----------------------------------------------

# to use the "join" functions we create a separate data frame containing the risk score
# the two data frames must be joined by a shared column so we create a column with row number in the data set

# create column with row number
pbc_data_clean <- pbc_data_clean %>% 
  mutate(row.number = rownames(.))

# create edema score column
pbc_data_clean <- pbc_data_clean %>% 
  mutate(edema.score = case_when(edema == "no edema" ~ 0,
                                 edema == "edema, no diuretic therapy" ~ 0.5,
                                 edema == "edema despite diuretic therapy" ~ 1)) %>% 
  relocate(edema.score, .after = edema)

# select the edema column in a new tibble
edema_col <- pbc_data_clean %>% 
  select(edema)

# calculate risk score
risk_score <- pbc_data_clean %>%
  mutate(mayo.risk = 
           0.04 * age + 
           10.87 * log(bili) - 
           22.53 * log(albumin) +
           12.38 * log(protime) +
           10.86 * edema.score) %>% 
  select(row.number, mayo.risk)

# convert to risk level
risk_score <- risk_score %>%
  mutate(
    mayo.risk.level = case_when(
      mayo.risk < 8.5 ~ "low risk",
      8.5 <= mayo.risk &
        mayo.risk <= 10 ~ "medium risk",
      mayo.risk > 10 ~ "high risk"
    )
  )

# join risk score with pbc data frame and remove row number column
pbc_data_clean <- left_join(pbc_data_clean, risk_score, by = "row.number") %>%
  select(-row.number)


# Mutate edema column -----------------------------------------------------
pbc_data_clean <- pbc_data_clean %>%
  mutate(diuretic = ifelse(edema == "edema despite diuretic therapy", 1, 0)) %>%
  mutate(edema = ifelse(edema == "no edema", 0, 1)) %>% 
  relocate(diuretic, .after = edema)


# Create end age variable -------------------------------------------------
pbc_data_clean <- pbc_data_clean %>% 
  mutate(end.age = floor(age + (fu.days/365.25))) %>% 
  relocate(end.age, .after = age)


# Write data --------------------------------------------------------------
write_csv(x = pbc_data_clean, file = "data/03_pbc_data_aug.csv")
