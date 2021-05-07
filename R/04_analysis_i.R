# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)

# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")

# Source functions --------------------------------------------------------
source("R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------
pbc_data_aug <- factor_columns(pbc_data_aug)

# Exploratory data analysis -----------------------------------------------

# Bar plots of data set overview
bar1 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = stage, fill = stage)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barplot of participants in different stages",
    x = "Disease stage",
    y = "Count"
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  ) 

bar2 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = drug, fill = drug)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barplot of participants in each treatment category",
    x = "Treatment",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  ) 
  
plt_bar <- bar1 | bar2 

plots <- c("plt_bar")

# Histograms of numeric variables
pbc_numberic <- pbc_data_aug %>%
  select(where(is.numeric), drug) %>%
  select(-edema,-diuretic,-edema.score,-mayo.risk)

plt_histogram <- ggplot(gather(pbc_numberic, key, value, -c(drug)),
       mapping = aes(value, fill = factor(drug))) +
  geom_histogram(bins = 10, alpha = 0.5) +
  #geom_histogram(mapping = aes(y = (..density..)), bins = 8) +
  facet_wrap(~ key, scales = 'free_x') +
  theme_classic() +
  labs(title = "Histograms of numeric attributes in the data set",
       x = "",
       y = "Count",
       caption = "Data from https://hbiostat.org/data/") +
  theme(
    plot.title = element_text(size = 30),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.x = element_text(
      size = 8,
      angle = 45,
      vjust = 1,
      hjust = 1)
  ) +
  scale_fill_discrete(name = "Drug")

plots <- c(plots, "plt_histogram")

# Box plot age vs. drug (stratified by sex)
box1 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = age, y = drug, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Distribution of age in the two drug treatments for both sex",
    x = "Age",
    y = "Drug"
  ) +
  theme(
    plot.title.position = "plot"
  ) +
  scale_fill_discrete(name = "Sex")

# Box plot age vs. Mayo risk score (stratified by sex)
box2 <- pbc_data_aug %>%
  group_by(mayo.risk.level) %>%
  ggplot(mapping = aes(x = age, y = mayo.risk.level, fill = sex)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Distribution of age in the three Mayo risk score levels for both sex",
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

plt_box_age <-
  box1 / box2 + 
  plot_annotation(title = "Boxplot of age stratified by different variables",
                                theme = theme(plot.title = element_text(hjust = 0.5, size = 20))) +
  plot_layout(guides = "collect")

plots <- c(plots, "plt_box_age")


# Distribution of participants in the three Mayo risk score categories
plt_bar_mayo <- pbc_data_aug %>%
  count(mayo.risk.level) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = mayo.risk.level, y = pct)) +
  geom_bar(stat = "identity", aes(fill = mayo.risk.level)) +
  theme_classic() +
  labs(
    title = "Distribution of participants in the three Mayo risk score levels",
    x = "Mayo risk score level",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/",
    fill = "Mayo risk level"
  ) +
  scale_fill_manual(labels = c("High risk: Mayo risk > 10", 
                               "Medium risk: 8.5 < Mayo risk > 10",
                               "Low risk: Mayo risk < 8.5"),
                    values = alpha(c("blue", "red", "green"), 0.3),
                    limits = rev) +
  theme(
    text = element_text(size= 20),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "right"
  )

plots <- c(plots, "plt_bar_mayo")

# Distribution of sex in the data set
plt_bar_sex <- pbc_data_aug %>%
  count(sex) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sex, y = pct)) +
  geom_bar(stat = "identity", aes(fill = sex)) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_classic() +
  labs(
    title = "Distribution of sex in the data",
    x = "Sex",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size=20),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  ) 

plots <- c(plots, "plt_bar_sex")

# Correlation between bilirubin and number of follow up days (stratified by sex and Mayo risk score)
point1 <- pbc_data_aug %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    color = mayo.risk.level,
    shape = sex
  )) +
  geom_point(size = 3) +
  theme_classic() +
  labs(
    title = "Correlation between Serum Bilirubin \n and number of follow-up days",
    x = "Follow-up days",
    y = "Serum Bilirubin (mg/dl)"
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  scale_shape_manual(values = c(15, 17), name = "Sex") +
  scale_color_manual(name = "Mayo risk score level", values = alpha(c("blue", "red", "green"), 0.5), limits = rev)

# Correlation between bilirubin and time to death (stratified by sex and Mayo risk score)
point2 <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    color = mayo.risk.level,
    shape = sex
  )) +
  geom_point(size = 3) +
  theme_classic() +
  labs(
    title = "Correlation between Serum Bilirubin \n and number of days to death",
    x = "Days to death",
    y = "Serum Bilirubin (mg/dl)",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_manual(name = "Mayo risk score level", values = alpha(c("blue", "red", "green"), 0.5), limit = rev) +
  scale_shape_manual(values = c(15, 17), name = "Sex")

plt_bili_scatter <- (point1 | point2) + plot_annotation(title = "Scatterplot of number of follow-up days",
                                theme = theme(plot.title = element_text(hjust = 0.5, size = 20)))

plots <- c(plots, "plt_bili_scatter")

# Histograms of the variable follow up days

# Histogram of time to death/liver transplant/end of study
h1 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = fu.days)) +
  geom_histogram(
    binwidth = 365.25,
    fill = "royalblue4",
    color = "royalblue4",
    alpha = 0.3
  ) +
  theme_classic() +
  labs(title = "All participants",
       x = "Follow-up days", y = "Count") +
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
                 alpha = 0.3) +
  theme_classic() +
  labs(title = "Participants who die",
       x = "Follow-up days", y = "Count") +
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
    alpha = 0.3
  ) +
  theme_classic() +
  labs(
    title = "Participants who live",
    x = "Follow-up days",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

plt_hist_fu <- h1 / (h2 + h3) + plot_annotation(title = "Histograms of number of follow-up days",
                                 theme = theme(plot.title = element_text(hjust = 0.5, size = 20)))

plots <- c(plots, "plt_hist_fu")

# Boxplot of the variable follow up days

# Boxplot of time to death/liver transplant/end of study
b1 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = fu.days, y = status)) +
  geom_boxplot(alpha = 0.3,
               fill = "royalblue4",
               color = "royalblue4") +
  theme_classic() +
  labs(title = "Number of follow-up days in relation to status",
       x = "Follow-up days", y = "Status") +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  )

# Boxplot of time to death (stratified by stage)
b2 <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(x = fu.days, y = stage)) +
  geom_boxplot(alpha = 0.3,
               fill = "red",
               color = "red") +
  theme_classic() +
  labs(title = "Participants who have died (stratified by stage)",
       x = "Follow-up days", y = "Stage") +
  theme(
    plot.title.position = "plot",
    legend.position = "none"
  )

# Boxplot of time to end of study (stratified by stage)
b3 <- pbc_data_aug %>%
  filter(status == 0) %>%
  ggplot(mapping = aes(x = fu.days, y = stage)) +
  geom_boxplot(alpha = 0.2,
               fill = "green4",
               color = "green4") +
  theme_classic() +
  labs(
    title = "Participants who live (stratified by stage)",
    x = "Follow-up days",
    y = "Stage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

plt_box_fu <- b1 / (b2 + b3) + plot_annotation(title = "Boxplots of number of follow-up days",
                                 theme = theme(plot.title = element_text(hjust = 0.5, size = 20)))

plots <- c(plots, "plt_box_fu")

# Boxplot of time to death (stratified by stage and drug)
box_fu1 <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(x = fu.days, y = stage, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Number of days to death (stratified by stage and drug)",
    x = "Days to death",
    y = "Stage"
  ) +
  theme(
    plot.title.position = "plot",
  ) +
  scale_fill_discrete(name = "Drug") +
  scale_x_continuous(limits = c(0,5000))

# Boxplot of time to death/liver transplant/end of study (stratified by status and drug)
box_fu2 <- pbc_data_aug %>%
  ggplot(mapping = aes(x = fu.days, y = status, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Number of follow-up days (stratified by status and drug)",
    x = "Follow-up days",
    y = "Status",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug") +
  scale_x_continuous(limits = c(0,5000))

plt_box_fu2 <- box_fu1 / box_fu2 + 
  plot_annotation(title = "Boxplots of number of follow-up days",
                                    theme = theme(plot.title = element_text(hjust = 0.5, size = 20))) +
  plot_layout(guides = "collect")

plots <- c(plots, "plt_box_fu2")

# Boxplot of time to death (stratified by status 1 and drug)
plt_box_fu3 <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(x = fu.days, y = status, fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Number of follow-up days (stratified by status 1 and drug)",
    x = "Follow-up days",
    y = "",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug") +
  scale_x_continuous(limits = c(0,5000))

plots <- c(plots, "plt_box_fu3")

# Barplot of number of participants for each status level (stratified by drug)
plt_bar_drug <- pbc_data_aug %>%
  ggplot(aes(x = status, fill = drug)) +
  geom_bar(position = "dodge", alpha = 0.5) +
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

plots <- c(plots, "plt_bar_drug")

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
  geom_step(aes(x = fu.days, y = (125 - cumulative_count) / 125))  +
  theme_classic() +
  labs(
    title = "Number of follow-up days (stratified by drug)",
    x = "Follow-up days",
    y = "Survival rate",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_discrete(name = "Drug")

plots <- c(plots, "plt_step_drug")

# Step plot of survival percentage over time (stratified by stage)
plt_step_stage <- pbc_data_aug %>%
  filter(status == 1) %>%
  group_by(stage) %>%
  arrange(fu.days) %>%
  ggplot(mapping = aes(x = fu.days, color = stage)) +
  geom_step(aes(x = fu.days, y = (125 - cumulative_count) / 125)) +
  theme_classic() +
  labs(
    title = "Number of follow-up days in relation to the 4 stages",
    x = "Follow-up days",
    y = "Survival rate",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    plot.caption = element_text(hjust = 1, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_discrete(name = "Stage")

plots <- c(plots, "plt_step_stage")

l <- mget(plots)

# Save plots --------------------------------------------------------------
invisible(mapply(
  ggsave,
  file = paste0("results/", names(l), ".png"),
  plot = l,
  width = 12,
  height = 6
))

