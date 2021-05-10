# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gt)


# Load data ---------------------------------------------------------------
pbc_data_clean <- read_csv("data/02_pbc_data_clean.csv")


# Calculate Mayo Risk score -----------------------------------------------

# To use the "join" functions we create a separate data frame 
# containing the risk score.
# The two data frames must be joined by a shared column 
# so we create a column with row number in the data set

# Create column with row number
pbc_data_clean <- pbc_data_clean %>% 
  mutate(row.number = rownames(.))

# Create edema score column
pbc_data_clean <- pbc_data_clean %>%
  mutate(
    edema.score = case_when(
      edema == "no edema" ~ 0,
      edema == "edema, no diuretic therapy" ~ 0.5,
      edema == "edema despite diuretic therapy" ~ 1
    )
  ) %>%
  relocate(edema.score, 
           .after = edema)

# Store the edema column in a new tibble for making table later
edema_col <- pbc_data_clean %>% 
  select(edema) %>% 
  slice(1:3) %>% # hard coding because it is for an example only 
  mutate(row.number = rownames(.))

# Calculate risk score
risk_score <- pbc_data_clean %>%
  mutate(mayo.risk = 
           0.04 * age + 
           10.87 * log(bili) - 
           22.53 * log(albumin) +
           12.38 * log(protime) +
           10.86 * edema.score) %>% 
  select(row.number, 
         mayo.risk)

# Convert to risk level
risk_score <- risk_score %>%
  mutate(
    mayo.risk.level = case_when(
      mayo.risk < 8.5 ~ "low risk",
      8.5 <= mayo.risk &
        mayo.risk <= 10 ~ "medium risk",
      mayo.risk > 10 ~ "high risk"
    )
  )

# Join risk score with pbc data frame and remove row number column
pbc_data_clean <- left_join(pbc_data_clean, risk_score, by = "row.number") %>%
  select(-row.number)


# Mutate edema column -----------------------------------------------------
pbc_data_clean <- pbc_data_clean %>%
  mutate(diuretic = ifelse(edema == "edema despite diuretic therapy", 1, 0)) %>%
  mutate(edema = ifelse(edema == "no edema", 0, 1)) %>%
  relocate(diuretic,
           .after = edema)


# Edema table -------------------------------------------------------------
edema_new <- pbc_data_clean %>%
  select(edema, diuretic, edema.score) %>%
  rename(edema_new = edema) %>%
  slice(1:3) %>%
  mutate(row.number = rownames(.))

edema_both <- edema_col %>%
  rename(edema_raw = edema) %>%
  full_join(., edema_new, by = "row.number") %>%
  select(-row.number)


table_edema <- edema_both %>%
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(columns = everything())) %>%
  tab_header(title = md("Edema columns in raw versus augmented data")) %>%
  tab_spanner(label = "Augmented data",
              columns = matches("edema_new|diuretic|edema.score"))


# Write data --------------------------------------------------------------
pbc_data_aug <- pbc_data_clean 

write_csv(x = pbc_data_aug, file = "data/03_pbc_data_aug.csv")


# Save in results ---------------------------------------------------------
table_edema %>% 
  gtsave(filename = "results/table_edema.png")

