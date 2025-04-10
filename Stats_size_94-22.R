rm(list=ls()) # clean up
if (!is.null(dev.list())) {
  dev.off()  # Close the current active device
  while (!is.null(dev.list())) {
    dev.off()  # Keep closing until all devices are closed
  }
}
# Load necessary package
library(tidyverse)
library(ggplot2)
library(viridis)
library(readr)
library(readxl)


# Load dataset
size_rawdata_94_22 <- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/All_Clam_Sizes_1994-2022.csv', header = TRUE)

# Convert Year and Island to factors
size_rawdata_94_22$Year <- as.factor(size_rawdata_94_22$Year)
size_rawdata_94_22$Island <- as.factor(size_rawdata_94_22$Island)

# ANOVA for mean giant clam size across years
anova_years <- aov(Size ~ Year, data = size_rawdata_94_22)
summary(anova_years)    #0.00000000965
#######################

# ANOVA for mean giant clam size across islands
anova_islands <- aov(Size ~ Island, data = size_rawdata_94_22)
summary(anova_islands)   #0.00000000000000002

size_rawdata_94_22 <- size_rawdata_94_22 %>%
  mutate(Year = as.character(Year),  # Keep Year as character to handle "1994/95"
         Year = ifelse(grepl("/", Year), NA, Year),  # Remove non-numeric years
         Year = as.numeric(Year),
         Island = as.factor(Island)) %>%
  drop_na(Year)  # Remove rows where Year is NA

# ANOVA for mean giant clam size within each island across years
results <- size_rawdata_94_22 %>%
  group_by(Island) %>%
  group_split() %>%
  map(~ aov(Size ~ Year, data = .x) %>% summary())

# Print results for each island
names(results) <- unique(size_rawdata_94_22$Island)
results       #0.00209

# --- ANOVA TESTS ---

# ANOVA for mean giant clam size across years
anova_years <- aov(Size ~ as.factor(Year), data = size_rawdata_94_22)
summary(anova_years)

# ANOVA for mean giant clam size across islands
anova_islands <- aov(Size ~ Island, data = size_rawdata_94_22)
summary(anova_islands)

# ANOVA for mean giant clam size within each island across years
anova_results <- size_rawdata_94_22 %>%
  group_by(Island) %>%
  group_split() %>%
  map(~ aov(Size ~ as.factor(Year), data = .x) %>% summary())

# Print ANOVA results per island
names(anova_results) <- unique(size_rawdata_94_22$Island)
anova_results

# --- PLOT MEAN SIZE ACROSS YEARS WITH ERROR BARS ---

# Compute mean and standard error per Year-Island
historical_size <- size_rawdata_94_22 %>%
  group_by(Year, Island) %>%
  summarise(Mean = mean(Size, na.rm = TRUE),
            SE = sd(Size, na.rm = TRUE) / sqrt(n()), .groups = "drop")

# Generate the plot
historical_size_plot <- ggplot(historical_size, aes(x = Year, y = Mean, group = Island, color = Island, shape = Island)) +
  geom_point(size = 5) +  # Points for each island-year
  geom_line(size = 1.2) +  # Connecting lines for trends
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +  # Standard error bars
  scale_color_viridis_d(name = "Island", option = "B") +  # Use Viridis color palette
  scale_shape_manual(values = c(16, 17, 18, 15, 8)) +  # Different shapes for each island
  scale_y_continuous(limits = c(3, 30), breaks = seq(5, 30, by = 5)) +  # Adjust Y-axis
  scale_x_continuous(breaks = seq(min(historical_size$Year), max(historical_size$Year), by = 2)) +  # Adjust X-axis
  labs(x = "Year", y = "Mean Clam Size (cm)") +  # Axis labels
  theme_minimal() +  # Minimal theme
  theme(
    legend.position = c(0.18, 0.95),
    legend.background = element_rect(color = "black", fill = "grey90", size = 0.5),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Print the plot
print(historical_size_plot)




size_rawdata_94_22 <- size_rawdata_94_22 %>%
  mutate(Year = as.character(Year),  # Keep Year as character to handle "1994/95"
         Year = ifelse(grepl("/", Year), NA, Year),  # Remove non-numeric years
         Year = as.numeric(Year),
         Island = as.factor(Island)) %>%
  drop_na(Year)  # Remove rows where Year is NA

# --- ANOVA TESTS ---

# ANOVA for mean giant clam size across years
anova_years <- aov(Size ~ as.factor(Year), data = size_rawdata_94_22)
summary(anova_years)

# ANOVA for mean giant clam size across islands
anova_islands <- aov(Size ~ Island, data = size_rawdata_94_22)
summary(anova_islands)


# --- PLOT MEAN SIZE ACROSS YEARS WITH ERROR BARS ---

# Compute mean and standard error per Year-Island
historical_size <- size_rawdata_94_22 %>%
  group_by(Year, Island) %>%
  summarise(Mean = mean(Size, na.rm = TRUE),
            SE = sd(Size, na.rm = TRUE) / sqrt(n()), .groups = "drop")

# Generate the plot
historical_size_plot <- ggplot(historical_size, aes(x = Year, y = Mean, group = Island, color = Island, shape = Island)) +
  geom_point(size = 5) +  # Points for each island-year
  geom_line(size = 1.2) +  # Connecting lines for trends
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +  # Standard error bars
  scale_color_viridis_d(name = "Island", option = "B") +  # Use Viridis color palette
  scale_shape_manual(values = c(16, 17, 18, 15, 8)) +  # Different shapes for each island
  scale_y_continuous(limits = c(3, 30), breaks = seq(5, 30, by = 5)) +  # Adjust Y-axis
  scale_x_continuous(breaks = seq(min(historical_size$Year), max(historical_size$Year), by = 2)) +  # Adjust X-axis
  labs(x = "Year", y = "Mean Clam Size (cm)") +  # Axis labels
  theme_minimal() +  # Minimal theme
  theme(
    legend.position = c(0.18, 0.95),
    legend.background = element_rect(color = "black", fill = "grey90", size = 0.5),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Print the plot
print(historical_size_plot)
