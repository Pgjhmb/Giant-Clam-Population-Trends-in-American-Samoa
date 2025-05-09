rm(list = ls())
if (!is.null(dev.list())) while (!is.null(dev.list())) dev.off()
# Load necessary libraries
library(ggplot2)
library(viridis)
library(readr)

# Create the data frame
historical_size <- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/1994-2022 Size Clams Overview.csv', header = TRUE)

# Convert Year to a factor for discrete x-axis
historical_size$Year <- as.factor(historical_size$Year)

# Define custom colors for the legend, ensuring Tutuila is yellow
island_colors <- viridis::viridis(n = length(unique(historical_size$Island)), option = "B")
names(island_colors) <- unique(historical_size$Island)
island_colors["Tutuila"] <- "yellow"  # Override Tutuila color

# Generate the base plot
historical_size_plot <- ggplot(historical_size, aes(x = Year, y = Mean, group = Island, color = Island, shape = Island)) +
  geom_point(size = 5) +  # Add points with different shapes per Island
  geom_line(size = 1.2) +  # Add connecting lines
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +  # Add error bars
  scale_color_viridis_d(name = "Island", option = "B") +  # Use viridis color palette for all islands
  scale_shape_manual(values = c(16, 17, 18, 15, 8)) +  # Assign unique shapes to each island
  scale_y_continuous(limits = c(3, 30), breaks = seq(5, 30, by = 5)) +  # Adjust y-axis
  scale_x_discrete(expand = c(0.1, 0.1)) +  # Push 1994/95 and 2022 closer to edges
  coord_cartesian(clip = "off") +  # Ensure text is fully visible outside the panel
  labs(x = "Year", y = "Mean Clam Size (cm)") +  # Update axis labels
  theme_minimal() +   
  theme(
    legend.position = c(0.18, 0.953),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "grey90", size = 0.5, linetype = "solid"),
    legend.text = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 14, face = "bold", color = "black"),
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(margin = margin(t = 10), size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(margin = margin(r = 11), size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Add a separate layer for Tutuila with a manual yellow override
historical_size_plot <- historical_size_plot + 
  geom_point(data = subset(historical_size, Island == "Tutuila"),
             aes(x = Year, y = Mean),
             size = 5, shape = 16, color = "yellow", stroke = 2, show.legend = FALSE)  # Prevents affecting legend


# Print the plot
print(historical_size_plot)
ggsave("/Users/Paolo/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Working R Scripts/historical_size_plot.jpg", plot = historical_size_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)


