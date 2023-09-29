# Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(grid)

setwd('C:/Users/utkge/Desktop/data/buiding_height')
# Read the CSV file
data <- read_csv("all_states_stats.csv",show_col_types = FALSE)

# View the first few rows of the data
head(data)

# Melt the data to long format
data_long <- data %>% pivot_longer(cols = c(Total, Count_MS, Count_FM), names_to = "variable", values_to = "value")

# Create a new y-axis scale variable
data_long$`value/million` <- ifelse(data_long$variable == "Count_FM", pmin(data_long$value / 10^6, 5), data_long$value / 10^6)


# Plot the data
p <- ggplot(data_long, aes(x = State, y = `value/million`, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = seq(1, nrow(data), by = 1), color = "grey", linetype = "dashed", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
  facet_grid(variable ~ ., scales = "free_y", space = "free", switch = "y") +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.5, "lines")) +
  guides(fill = guide_legend(title = NULL))

p

# Rename the columns
data <- data %>%
  rename(ExtraHeight_FM = `Height Avg Not Null`)

# Calculate the percentage of ExtraHeight_FM
data <- data %>%
  mutate(ExtraHeight_Percentage = ExtraHeight_FM / Total * 100)

data$ExtraHeight_FM_Thousands <- data$ExtraHeight_FM / 100000

# Bar chars
# Bar plot with adjusted y values, label, and no space between 0 and x-axis
p2 <- ggplot(data, aes(x = State, y = ExtraHeight_FM_Thousands)) +
  geom_bar(stat = "identity",fill = 'lightblue') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +  # Removing the default expansion
  labs(
    y = "Buildings with Height in FEMA but not in MS (x1,000)",
    x = "",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
    legend.position = "none"
  )

p2


# Line plot of Mean_height by State
p <- ggplot(data, aes(x = State, y = Mean_height)) +
  geom_line(aes(group = 1), color = "blue") +
  geom_point(color = "blue") + # Adding points to make the individual values stand out
  labs(
    y = "Mean Height (m)",
    x = "",
    title = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12)
  )

p
