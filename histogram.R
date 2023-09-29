library(tidyverse)
library(viridis)

# Read the CSV file
csv_path <- "E:/building_height/data/height_distribution.csv"
df <- read.csv(csv_path)

# Rename the columns and remove '0-1' column
colnames(df) <- c("filename", "0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", ">10")
df$`0-1` <- NULL

df <- read.csv(csv_path)

# Rename the columns and remove '0-1' column
colnames(df) <- c("filename", "0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", ">10")
df$`0-1` <- NULL

# Melt the data frame into a long format
long_df <- df %>%
  gather(key = 'height_interval', value = 'count', -filename)

# Convert height_interval to a factor with specific order
ordered_intervals <- c("1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", ">10")
long_df$height_interval <- factor(long_df$height_interval, levels = ordered_intervals)

new_legend_names <- c("Cook, IL", "Eastern, MA", "King, WA", "Maricopa and Pinal, AZ", "Orange, FL", "Sata Clara, CA")

# Plotting using ggplot2
ggplot(long_df, aes(x = height_interval, y = count/1000, fill = filename)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       x = "Height Interval (m)",
       y = "Count (in thousands)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12)
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis(discrete = TRUE, labels = new_legend_names)