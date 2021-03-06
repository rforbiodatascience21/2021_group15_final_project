# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl) # a part of the tidyverse installation


# Load data ---------------------------------------------------------------
pbc_data <- read_xlsx("data/_raw/pbc.xlsx")


# Write data --------------------------------------------------------------
write_csv(x = pbc_data,
          file = "data/01_pbc_data.csv")
