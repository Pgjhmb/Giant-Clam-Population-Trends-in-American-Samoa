rm(list=ls()) # clean up
if (!is.null(dev.list())) {
  dev.off()  # Close the current active device
  while (!is.null(dev.list())) {
    dev.off()  # Keep closing until all devices are closed
  }
}


# Load necessary libraries
library(lmtest)
library(tidyverse)
library(mvabund)
library(hrbrthemes)
library(viridis)

# Read and process the data
clamsize2 <- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2022-2024_All_Clams_Measured.csv', header = TRUE)

# Set the desired order for Island and define size classes
clamsize2 <- clamsize2 %>%
  mutate(Island = factor(Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Rose")),
         SizeClass = factor(case_when(
           Size <= 5 ~ "Recruit",
           Size > 5 & Size <= 11 ~ "Immature",
           Size >= 12 ~ "Mature"
         ), levels = c("Mature", "Immature", "Recruit")))  # Flip the order here

# Summarize data by Island and SizeClass
clamsize2_summary <- clamsize2 %>%
  group_by(Island, SizeClass) %>%
  summarise(Count = n(),
            mean_clams_size = mean(Size),
            se_clams = sd(Size) / sqrt(n()), .groups = "drop")

# Calculate cumulative heights for each segment and add custom offsets for specific islands
reshaped_counts_long <- clamsize2_summary %>%
  mutate(SizeClass = factor(SizeClass, levels = c("Mature", "Immature", "Recruit"))) %>%  # Ensure flipped order here as well
  group_by(Island) %>%
  arrange(SizeClass) %>%
  mutate(cumulative_height = cumsum(Count)) %>%
  # Add different y offsets for specific islands and size classes in a single mutate
  mutate(y_offset = case_when(
    Island == "Aunu‘u" & SizeClass == "Recruit" ~ cumulative_height + 3,
    Island == "Aunu‘u" & SizeClass == "Immature" ~ cumulative_height + 14,
    Island == "Aunu‘u" & SizeClass == "Mature" ~ cumulative_height + 22,
    Island == "Ofu" & SizeClass == "Recruit" ~ cumulative_height + 3,
    Island == "Ofu" & SizeClass == "Immature" ~ cumulative_height + 15,
    Island == "Ofu" & SizeClass == "Mature" ~ cumulative_height + 24,
    Island == "Olosega" & SizeClass == "Recruit" ~ cumulative_height + 3,
    Island == "Olosega" & SizeClass == "Immature" ~ cumulative_height + 11,
    Island == "Olosega" & SizeClass == "Mature" ~ cumulative_height + 19,
    TRUE ~ NA_real_  # Set to NA for all other cases
  ))

# Create the stacked bar plot with count labels
stacked_bar_plot <- ggplot(reshaped_counts_long, aes(x = Island, y = Count, fill = SizeClass)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Mature" = "grey40", "Immature" = "white", "Recruit" = "black")) +
  labs(x = "Island", y = "Number of Clams") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey90"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 12, color = "black", margin = margin(t = 2.3, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(face = "bold", size = 12, color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(face = "bold", size = 13)
  ) +
  # Add labels above the bars for specific islands with different offsets
  geom_label(data = reshaped_counts_long %>% filter(Island %in% c("Aunu‘u", "Ofu", "Olosega")),
             aes(x = Island, y = y_offset, label = Count, color = SizeClass, fill = SizeClass),
             fontface = "bold", size = 3.5, label.size = 0, inherit.aes = FALSE) +
  # Add count labels within each bar segment for all other islands
  geom_label(data = reshaped_counts_long %>% filter(!Island %in% c("Aunu‘u", "Ofu", "Olosega")),
             aes(label = Count, color = SizeClass), position = position_stack(vjust = 0.5), fontface = "bold", size = 3.5, label.size = 0) +
  scale_color_manual(values = c("Mature" = "white", "Immature" = "black", "Recruit" = "white")) +
  scale_fill_manual(values = c("Mature" = "grey40", "Immature" = "white", "Recruit" = "black"))

# Display the plot
print(stacked_bar_plot)

ggsave("~/Desktop/Island_sizeClass_StackedBar.jpg", plot = stacked_bar_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)
