# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# Exploratory data analysis -----------------------------------------------

drug_boxplot <- pbc_data_aug %>% 
  ggplot(mapping = aes(x = age, y = drug, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

pbc_data_aug %>% 
  group_by(mayo_risk_level) %>% 
  ggplot(mapping = aes(x = age, y = mayo_risk_level, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

pbc_data_aug %>% 
  count(mayo_risk_level) %>% 
  mutate(pct = n/ sum(n) * 100) %>% 
  ggplot(aes(x = mayo_risk_level, y = pct)) +
  geom_col()

pbc_data_aug %>% 
  count(sex) %>% 
  mutate(pct = n/ sum(n) * 100) %>% 
  ggplot(aes(x = sex, y = pct)) +
  geom_col()

# bilirubin and fu days
pbc_data_aug %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    colour = mayo_risk_level,
    shape = sex
  )) +
  geom_point() +
  scale_shape_manual(values=c(3, 19))


# follow up days histogram
pbc_data_aug %>% 
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(binwidth = 365.25) +
  theme_classic()

# survival time

# get row names ordered by number of follow up dayes
cumulative_count <- pbc_data_aug %>% 
  filter(status == 1) %>% 
  arrange(fu.days) %>% 
  rownames() %>% 
  as.numeric()

# plot survival percentage over follow up time
pbc_data_aug %>% 
  filter(status == 1) %>% 
  arrange(fu.days) %>%
  ggplot(mapping = aes(x= fu.days)) + 
  geom_step(aes(x=fu.days,y=(312-cumulative_count)/312)) 
  

pbc_data_aug %>% rownames() %>% as.numeric()

library(ggcorrplot)

pbc_data_aug %>% select_if(is.numeric) %>% 
  cor(., method = "pearson") %>% 
  ggcorrplot(., hc.order=T, lab=T, type="lower",
             colors = c("#6D9EC1", "white", "#E46726"),
             legend.title = "'Pearson's\nCorrelation",
             lab_size = 3) +
  theme(legend.title=element_text(size=8), 
        legend.text=element_text(size=8),
        title = element_text(size=10),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  labs(title = "Correlation matrix") 




