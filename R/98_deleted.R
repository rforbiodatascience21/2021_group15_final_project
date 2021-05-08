# Plots looking at survival

# get row names ordered by number of follow up days
cumulative_count <- pbc_data_aug %>%
  filter(status == 1) %>%
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