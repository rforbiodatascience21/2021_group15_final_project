# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)

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

# List for storing plots
plots <- NA

# Histograms of numeric variables
pbc_numberic <- pbc_data_aug %>%
  select(where(is.numeric), drug) %>%
  select(-stage,-status,-edema,-diuretic,-edema_score,-mayo_risk)

ggplot(gather(pbc_numberic, key, value, -c(drug)),
       mapping = aes(value, fill = factor(drug))) +
  geom_histogram(bins = 10) +
  facet_wrap(~ key, scales = 'free_x') +
  theme_classic() +
  labs(title = "Histograms of numeric attributes in the data set",
       y = "Count",
       caption = "Data from https://hbiostat.org/data/") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug")

# Box plot age vs. drug (stratified by sex)
pbc_data_aug %>%
  ggplot(mapping = aes(x = age, y = drug, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Distribution of age in the two drug treatments for both sex",
    x = "Age",
    y = "Drug",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Sex")

# Box plot age vs. Mayo risk score (stratified by sex)
pbc_data_aug %>%
  group_by(mayo_risk_level) %>%
  ggplot(mapping = aes(x = age, y = mayo_risk_level, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Distribution of age in the three Mayo risk score levels for both sex,",
    x = "Age",
    y = "Mayo risk score level",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Sex")


# Distribution of participants in the three Mayo risk score categories
pbc_data_aug %>%
  count(mayo_risk_level) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = mayo_risk_level, y = pct)) +
  geom_bar(stat = "identity", alpha = 0.5, aes(fill = mayo_risk_level)) +
  theme_classic() +
  labs(
    title = "Distribution of participants in the three Mayo risk score levels",
    x = "Mayo risk score level",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  ) +
  scale_fill_discrete(limits = rev)

# Distribution of sex in the data set
pbc_data_aug %>%
  count(sex) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sex, y = pct)) +
  geom_bar(stat = "identity", alpha = 0.5, aes(fill = sex)) +
  theme_classic() +
  labs(
    title = "Distribution of sex in the data",
    x = "Sex",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# Correlation between bilirubin and number of follow up days (stratified by sex and Mayo risk score)
pbc_data_aug %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    color = mayo_risk_level,
    shape = sex
  )) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Correlation between Serum Bilirubin and Time to Death or Liver Transplantation",
    x = "Time to Death or Liver Transplantation",
    y = "Serum Bilirubin (mg/dl)",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_shape_manual(values = c(3, 19), name = "Sex") +
  scale_color_discrete(name = "Mayo risk score level", limits = rev)

# Correlation between bilirubin and time to death (stratified by sex and Mayo risk score)
pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    color = mayo_risk_level,
    shape = sex
  )) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Correlation between Serum Bilirubin and Time to Death or Liver Transplantation",
    x = "Time to Death",
    y = "Serum Bilirubin (mg/dl)",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_shape_manual(values = c(3, 19), name = "Sex") +
  scale_color_discrete(name = "Mayo risk score level", limits = rev)

# Histograms of the variable follow up days

# Histogram of time to death/liver transplant/end of study
h1 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(
    binwidth = 365.25,
    fill = "royalblue4",
    color = "royalblue4",
    alpha = 0.5
  ) +
  theme_classic() +
  labs(title = "Time to Death or Liver Transplantation",
       x = "Time to Death or Liver Transplantation", y = "Count") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# Histogram of time to death
h2 <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(binwidth = 365.25,
                 aes(fill = "red", color = "red"),
                 alpha = 0.5) +
  theme_classic() +
  labs(title = "Participants with status 1",
       x = "Time to Death or Liver Transplantation", y = "Count") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# Histogram of time to liver transplant or end of study
h3 <- pbc_data_aug %>%
  filter(status == 0) %>%
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(
    binwidth = 365.25,
    fill = "green4",
    color = "green4",
    alpha = 0.5
  ) +
  theme_classic() +
  labs(
    title = "Participants with status 0",
    x = "Time to Death or Liver Transplantation",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

h1 / (h2 + h3)


# Boxplot of the variable follow up days

# Boxplot of time to death/liver transplant/end of study
b1 <- pbc_data_aug %>%
  mutate(status = factor(status, levels = c(0, 1))) %>%
  ggplot(mapping = aes(x = fu.days, y = status)) +
  geom_boxplot(alpha = 0.5,
               fill = "royalblue4",
               color = "royalblue4") +
  theme_classic() +
  labs(title = "Time to Death or Liver Transplantation in relation to status",
       x = "Time to Death or Liver Transplantation", y = "Status") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# Boxplot of time to death (stratified by stage)
b2 <- pbc_data_aug %>%
  filter(status == 1) %>%
  mutate(stage = factor(stage, levels = c(1, 2, 3, 4))) %>%
  ggplot(mapping = aes(x = fu.days, y = stage)) +
  geom_boxplot(alpha = 0.5,
               fill = "red",
               color = "red") +
  theme_classic() +
  labs(title = "Participants in status 1 in stage 1-4",
       x = "Time to Death or Liver Transplantation", y = "Stage") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# Boxplot of time to end of study (stratified by stage)
b3 <- pbc_data_aug %>%
  filter(status == 0) %>%
  mutate(stage = factor(stage, levels = c(1, 2, 3, 4))) %>%
  ggplot(mapping = aes(x = fu.days, y = stage)) +
  geom_boxplot(alpha = 0.5,
               fill = "green4",
               color = "green4") +
  theme_classic() +
  labs(
    title = "Participants in status 0 in stage 1-4",
    x = "Time to Death or Liver Transplantation",
    y = "Stage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

b1 / (b2 + b3)

# Boxplot of time to death (stratified by stage and drug)
pbc_data_aug %>%
  filter(status == 1) %>%
  mutate(stage = factor(stage, levels = c(1, 2, 3, 4))) %>%
  ggplot(mapping = aes(x = fu.days, y = stage, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Time to Death or Liver Transplantation at stage 1 to 4 in relation to the two drugs",
    x = "Time to Death or Liver Transplantation",
    y = "Stage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug")

# Boxplot of time to death/liver transplant/end of study (stratified by status and drug)
pbc_data_aug %>%
  mutate(status = factor(status, levels = c(0, 1))) %>%
  ggplot(mapping = aes(x = fu.days, y = status, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Time to Death or Liver Transplantation at status 0 or 1 in relation to the two drugs",
    x = "Time to Death or Liver Transplantation",
    y = "Status",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug")

# Barplot of number of participants for each status level (stratified by drug)
pbc_data_aug %>%
  mutate(status = factor(status, levels = c(0, 1))) %>%
  ggplot(aes(x = status, fill = drug)) +
  geom_bar(position = "dodge") +
  theme_classic() +
  labs(
    title = "Number of participants in status 0 or 1 in relation to the two drugs",
    x = "Status",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug")


# Scatterplot of prothrombin time vs. platelets (stratified by stage)
pbc_data_aug %>%
  mutate(stage = factor(stage, levels = c(1, 2, 3, 4))) %>%
  ggplot(mapping = aes(x = platelet, y = protime, color = stage)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Correlation between Platelets and Prothrombin Time at stages 1 to 4",
    x = "Platelets (per cm^3/1000)",
    y = "Prothrombin Time (sec.)",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Stage")

# Barplot of spiders variable
pbc_data_aug %>%
  ggplot(mapping = aes(x = spiders, fill = status)) +
  geom_bar(position = "dodge")  +
  theme_classic() +
  labs(
    title = "Count of participants with and without spiders",
    x = "Spiders",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Stage")


# Plots looking at survival

# get row names ordered by number of follow up days
cumulative_count <- pbc_data_aug %>%
  filter(status == 1) %>%
  arrange(fu.days) %>%
  rownames() %>%
  as.numeric()

# Step plot of survival percentage over time (stratified by drug)
plt_step_drug <- pbc_data_aug %>%
  filter(status == 1) %>%
  arrange(fu.days) %>%
  ggplot(mapping = aes(x = fu.days, color = drug)) +
  geom_step(aes(x = fu.days, y = (312 - cumulative_count) / 312))  +
  theme_classic() +
  labs(
    title = "Time to Death or Liver Transplantation in relation to the drugs",
    x = "Time to Death or Liver Transplantation",
    y = "Cumulative count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_discrete(name = "Drug")

plots <- c("plt_step_drug")

# Step plot of survival percentage over time (stratified by stage)
plt_step_stage <- pbc_data_aug %>%
  mutate(stage = factor(stage, levels = c(1, 2, 3, 4))) %>%
  filter(status == 1) %>%
  arrange(fu.days) %>%
  ggplot(mapping = aes(x = fu.days, color = stage)) +
  geom_step(aes(x = fu.days, y = (312 - cumulative_count) / 312)) +
  theme_classic() +
  labs(
    title = "Time to Death or Liver Transplantation in relation to the 4 stages",
    x = "Time to Death or Liver Transplantation",
    y = "Cumulative count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_discrete(name = "Stage")

plots <- c(plots, "plt_step_stage")

pbc_data_aug %>%
  group_by(status) %>%
  count(stage)


#Correlation plot. Not to be used in the presentation but just for exploring
library(ggcorrplot)

pbc_data_aug %>% select_if(is.numeric) %>%
  cor(., method = "pearson") %>%
  ggcorrplot(
    .,
    hc.order = T,
    lab = T,
    type = "lower",
    colors = c("#6D9EC1", "white", "#E46726"),
    legend.title = "'Pearson's\nCorrelation",
    lab_size = 3
  ) +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    title = element_text(size = 10)
  ) +
  theme_classic() +
  labs(title = "Correlation matrix") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(
      size = 8,
      angle = 45,
      vjust = 1,
      hjust = 1
    )
  ) +
  labs(caption = "Data from https://hbiostat.org/data/") +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )


l <- mget(plots)

# Save plots --------------------------------------------------------------
invisible(mapply(
  ggsave,
  file = paste0("results/", names(l), ".png"),
  plot = l,
  width = 5,
  height = 3
))

