# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# TEMPORARY - FIX FACTOR PROBLEM
pbc_data_aug <- pbc_data_aug %>%
  mutate(mayo_risk_level = factor(mayo_risk_level,
                                  levels =  c("low risk",
                                              "medium risk",
                                              "high risk")))

pbc_data_aug <- pbc_data_aug %>% 
  mutate(sex = factor(sex, levels = c("male", "female")))

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


# % in each may risk column
pbc_data_aug %>% 
  count(mayo_risk_level) %>% 
  mutate(pct = n/ sum(n) * 100) %>% 
  ggplot(aes(x = mayo_risk_level, y = pct)) +
  geom_bar(stat = "identity")

# % of each sex
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

pbc_data_aug %>% 
  filter(status == 1) %>% 
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(binwidth = 365.25) +
  theme_classic()

pbc_data_aug %>% 
  filter(status == 0) %>% 
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(binwidth = 365.25) +
  theme_classic()

pbc_data_aug %>% 
  mutate(status = factor(status, levels = c(0,1))) %>% 
  ggplot(mapping = aes(x = fu.days, y = status)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()


pbc_data_aug %>%
  filter(status == 1) %>% 
  mutate(stage = factor(stage, levels = c(1,2,3,4))) %>% 
  ggplot(mapping = aes(x = fu.days, y = stage)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()


pbc_data_aug %>%
  filter(status == 1) %>% 
  mutate(stage = factor(stage, levels = c(1,2,3,4))) %>% 
  ggplot(mapping = aes(x = fu.days, y = stage, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()


pbc_data_aug %>%
  mutate(status = factor(status, levels = c(0,1))) %>% 
  ggplot(mapping = aes(x = fu.days, y = status, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()




pbc_data_aug %>% 
  mutate(status = factor(status, levels = c(0,1))) %>%
  ggplot(aes(x = status, fill = drug)) +
  geom_bar(position = "dodge")



pbc_data_aug %>% 
  ggplot(mapping = aes(x = platelet, y = protime, color = stage)) +
  geom_point()


pbc_data_aug %>% 
  ggplot(mapping = aes(x = spiders, fill = status)) +
  geom_bar(position = "dodge")
  




# survival time

# get row names ordered by number of follow up days
cumulative_count <- pbc_data_aug %>% 
  filter(status == 1) %>% 
  arrange(fu.days) %>% 
  rownames() %>% 
  as.numeric()

# plot survival percentage over follow up time
pbc_data_aug %>% 
  filter(status == 1) %>% 
  arrange(fu.days) %>%
  ggplot(mapping = aes(x= fu.days, color = drug)) + 
  geom_step(aes(x=fu.days,y=(312-cumulative_count)/312)) 


pbc_data_aug %>%
  mutate(stage = factor(stage, levels = c(1,2,3,4))) %>% 
  filter(status == 1) %>% 
  arrange(fu.days) %>%
  ggplot(mapping = aes(x= fu.days, color = stage)) + 
  geom_step(aes(x=fu.days,y=(312-cumulative_count)/312))

pbc_data_aug %>%
  group_by(status) %>% 
  count(stage)


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




