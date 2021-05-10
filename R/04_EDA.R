# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork) # adding plots together


# Load data ---------------------------------------------------------------
pbc_data_aug <- read_csv("data/03_pbc_data_aug.csv")


# Source functions --------------------------------------------------------
source("R/99_project_functions.R")


# Wrangle data ------------------------------------------------------------
pbc_data_aug <- factor_columns(pbc_data_aug)


# Pre plotting prep -------------------------------------------------------

# Create empty vector for storing plot names
plots <- c()

# Create list for changing variable names in plots
variable_labs <- c(
  "Serum bilirubin (mg/dL)",
  "Albumin (mg/dL)",
  "Disease stage",
  "Prothrombin time (seconds)",
  "Sex",
  "Follow-up days",
  "Age (years)",
  "Spiders",
  "Hepatom",
  "Ascites",
  "Alkaline phosphatase (U/L)",
  "SGOT (U/mL)",
  "Serum cholesterol (mg/dL)",
  "Triglicerides (mg/dL)",
  "Plateles (per cubic ml/1000)",
  "Drug",
  "Status",
  "Edema",
  "Diuretic",
  "Edema score",
  "Urine copper (ug/day)",
  "Mayo risk score",
  "Mayo risk level"
)
names(variable_labs) <- c(
  "bili",
  "albumin",
  "stage",
  "protime",
  "sex",
  "fu.days",
  "age",
  "spiders",
  "hepatom",
  "ascites",
  "alk.phos",
  "sgot",
  "chol",
  "trig",
  "platelet",
  "drug",
  "status",
  "edema",
  "diuretic",
  "edema.score",
  "copper",
  "mayo.risk",
  "mayo.risk.level"
)


# Bar plots of data set overview ------------------------------------------
bar1 <- pbc_data_aug %>%
  ggplot(mapping = aes(
    x = stage, 
    fill = stage)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barplot of participants in different stages",
    x = "Disease stage",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20),
    plot.title.position = "plot",
    legend.position = "none"
  ) 

bar2 <- pbc_data_aug %>%
  ggplot(mapping = aes(
    x = drug, 
    fill = drug)) +
  geom_bar(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barplot of participants in each treatment category",
    x = "Treatment",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  ) 
  
plt_bar <- bar1 | bar2 

# Append plot name to plots list
plots <- c(plots, 
           "plt_bar")


# Histograms of numeric variables -----------------------------------------

# Make tibble with all numeric columns and drug column
# and remove columns that weren't in the original data set
pbc_numeric <- pbc_data_aug %>%
  select(where(is.numeric), 
         drug) %>%
  select(-c(edema,
         diuretic,
         edema.score,
         mayo.risk)) %>%
  pivot_longer(cols = -drug, 
               names_to = "key", 
               values_to = "value")

plt_histogram <- pbc_numeric %>%
  ggplot(mapping = aes(
    x = value, 
    color = factor(drug))) +
  geom_freqpoly(alpha = 0.5, 
                bins = 30, 
                size = 2) +
  facet_wrap( ~ key,
              scales = 'free',
              labeller = labeller(key = variable_labs)) +
  theme_classic() +
  labs(
    title = "Histograms of numeric attributes in the data set",
    x = "",
    y = "Count",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 14),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    )
  )  +
  scale_color_discrete(name = "Drug")

plots <- c(plots, 
           "plt_histogram")


# Bar plot with Mayo risk score -------------------------------------------
plt_bar_mayo <- pbc_data_aug %>%
  count(mayo.risk.level) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(mapping = aes(
    x = mayo.risk.level, 
    y = pct, 
    fill = mayo.risk.level)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    title = "Distribution of participants in the three Mayo risk score levels",
    x = "",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/",
    fill = "Mayo risk level"
  ) +
  scale_fill_manual(
    labels = c(
      "High risk:\nMayo risk score > 10",
      "Medium risk:\n8.5 < Mayo risk score > 10",
      "Low risk:\nMayo risk score < 8.5"
    ),
    name = "Mayo risk score level",
    values = alpha(c("red", 
                     "blue", 
                     "green"), 
                   0.3),
    limits = rev
  ) +
  theme(
    text = element_text(size = 25),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
  ) +
  scale_x_discrete(labels = c(
    "low risk" = "Low risk",
    "medium risk" = "Medium risk",
    "high risk" = "High risk"
  ))

plots <- c(plots, 
           "plt_bar_mayo")


# Bar plot with sex -------------------------------------------------------
plt_bar_sex <- pbc_data_aug %>%
  count(sex) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(mapping = aes(
    x = sex,
    y = pct,
    fill = sex)) +
  geom_bar(stat = "identity",
           alpha = 0.5) +
  scale_fill_discrete(limits = rev) +
  theme_classic() +
  labs(
    title = "Distribution of sex in the data",
    x = "Sex",
    y = "Percentage",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 30),
    plot.caption = element_text(hjust = 1,
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  ) +
  scale_x_discrete(labels = c("male" = "Male",
                              "female" = "Female"))

plots <- c(plots, 
           "plt_bar_sex")


# Scatterplot of bilirubin and follow-up days -----------------------------

# Scatterplot with all participants
point1 <- pbc_data_aug %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = bili,
    color = mayo.risk.level,
    shape = sex
  )) +
  geom_point(size = 3) +
  theme_classic() +
  labs(title = "Correlation between bilirubin\nand follow-up days",
       x = "Follow-up days",
       y = "Serum Bilirubin (mg/dL)") +
  theme(
    text = element_text(size = 15),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(name = "Mayo risk score level",
                     values = alpha(c("red", 
                                      "blue", 
                                      "green"), 
                                    0.3),
                     limits = rev)

# Scatter plot with participants who die
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
    title = "Correlation between bilirubin\nand days to death",
    x = "Days to death",
    y = "",
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 15),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_color_manual(
    labels = c(
      "high risk" = "High risk",
      "medium risk" = "Medium risk",
      "low risk" = "Low risk"
    ),
    name = "Mayo risk score level",
    values = alpha(c("red", 
                     "blue", 
                     "green"), 
                   0.3),
    limit = rev
  ) +
  scale_shape_manual(
    labels = c("male" = "Male",
               "female" = "Female"),
    values = c(15, 
               17),
    name = "Sex"
  )

plt_bili_scatter <- point1 | point2

plots <- c(plots, 
           "plt_bili_scatter")


# Boxplot with follow-up days --------------------------------------------
plt_box_fu <- pbc_data_aug %>%
  ggplot(mapping = aes(
    x = fu.days,
    y = stage, 
    fill = status)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(title = "Boxplot of follow-up days",
       x = "Follow-up days", 
       y = "Stage",
       caption = "Data from https://hbiostat.org/data/") +
  theme(text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, 
                                    face = "italic"),
        plot.title.position = "plot",
        plot.caption.position = "plot") +
  scale_fill_discrete(labels = c("0" = "Alive",
                                 "1" = "Dead"),
                      name = "Status")

plots <- c(plots, 
           "plt_box_fu")


# Plots of drug distribution --------------------------------------------

# Box plot of days to death
plt_box_drug <- pbc_data_aug %>%
  filter(status == 1) %>%
  ggplot(mapping = aes(
    x = fu.days, 
    y = status, 
    fill = drug)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Boxplot of days to death",
    x = "Days to death", 
    y = "", 
    caption = "Data from https://hbiostat.org/data/"
  ) +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_fill_discrete(name = "Drug",
                      labels = c("placebo" = "Placebo")) +
  scale_x_continuous(limits = c(0, 
                                5000))

# Barplot of status
plt_bar_drug <- pbc_data_aug %>%
  ggplot(aes(
    x = status, 
    fill = drug)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) +
  theme_classic() +
  labs(
    title = "Barplot of drug distribution",
    x = "Status",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20),
    plot.caption = element_text(hjust = 1, 
                                face = "italic"),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  scale_fill_discrete(name = "Drug", 
                      labels = c("placebo" = "Placebo")) +
  scale_x_discrete(labels = c("0" = "Alive", 
                              "1" = "Dead"))

plt_drug <- plt_bar_drug | plt_box_drug 

plots <- c(plots, 
           "plt_drug")


# Save plots --------------------------------------------------------------

# Get all plots
l <- mget(plots)

# Save 
invisible(mapply(
  ggsave,
  file = paste0("results/", 
                names(l), 
                ".png"),
  plot = l,
  width = 12,
  height = 6
))

