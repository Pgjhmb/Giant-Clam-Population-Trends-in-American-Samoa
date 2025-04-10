rm(list=ls()) # clean up
if (!is.null(dev.list())) {
  dev.off()  # Close the current active device
  while (!is.null(dev.list())) {
    dev.off()  # Keep closing until all devices are closed
  }
}
library(lmtest)
library(tidyverse)
library(mvabund)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(ggpubr)
library(stats)
library(rstatix)

Jurisdiction<- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2024 Survey Feb/2024 Faisua Surveys AS.csv', header = TRUE)
Jurisdiction[47:54, 3] <- "Ta‘ū"
Jurisdiction[, 3] <- as.character(Jurisdiction[, 3])
# Now assign the value to row 39, column 3
Jurisdiction[39, 3] <- "Aunu‘u" 
#view(Jurisdiction)


custom_colors <- scale_fill_manual(values = c(
  "Federal No Take" = "#0D0887FF",
  "None" = "#CC4678FF",
  "Remote" = "#F89441FF",
  "Inaccessible" = "black",
  "Village Protected" = "#F0F921FF"
))


# Load the data
Protection_data <- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2024 Survey Feb/2024 Faisua Surveys AS.csv', header = TRUE, fileEncoding = "ISO-8859-1")
Clam_hect <- Protection_data$Clams...Hectare
# Select relevant columns

Protection_data$Protection <- as.factor(Protection_data$Protection)
# Perform ANOVA
anova_result <- aov(Clam_hect ~ Protection, data = Protection_data)
# Display the summary of the ANOVA test
summary(anova_result)

Protection_data2 <- Protection_data %>%
  select(Protection, Clam_hect) %>%
  filter(!is.na(Protection) & !is.na(Clam_hect))

# Check data normality for choosing appropriate test
shapiro_test <- shapiro.test(Clam_hect)

# Perform Kruskal-Wallis test if data is non-normal
if(shapiro_test$p.value < 0.05) {
  kruskal_result <- kruskal.test(Clam_hect ~ Protection, data = Protection_data)
  print(kruskal_result)
  
  # Pairwise Wilcoxon test for non-parametric pairwise comparisons
  pairwise <- Protection_data %>%
    wilcox_test(Clam_hect ~ Protection, p.adjust.method = "bonferroni")
  print(pairwise)
  
} else {
  # Perform ANOVA if data is normally distributed
  anova_result <- aov(Clam_hect ~ Protection, data = data)
  print(summary(anova_result))
  
  # Tukey's HSD test for pairwise comparisons
  pairwise <- TukeyHSD(anova_result)
  print(pairwise)
}


stats <- list(
  stat_compare_means(label = "p.signif", label.y = 19,  # Display significance stars (p-value at y=22)
                     method = "anova"),  # Overall ANOVA test
  
  # Add pairwise comparisons between protection levels
  stat_compare_means(comparisons = list(c("None", "Federal No Take"),
                                        c("None", "Remote"),
                                        c("None", "Village Protected"),
                                        c("Federal No Take", "Remote"),
                                        c("Federal No Take", "Village Protected"),
                                        c("Remote", "Village Protected")),
                     method = "t.test", 
                     label = "p.signif", 
                     label.y = c(1300, 2000, 4000, 5000, 6000, 6500))
)

# Plot the comparisons for visualization
test <- ggplot(Protection_data, aes(x = Protection, y = Clam_hect, fill = Protection)) +   
  geom_boxplot() +   
  labs(
    x = "Protection Level", 
    y = expression(" Mean Density (number ha"^-1*")"), 
    color = "black"
  ) +   
  custom_colors +   
  theme_minimal() +   
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9, face = "bold", color = "black"),   
    legend.title = element_text(size = 13, face = "bold", color = "black"),  
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(margin = margin(t = 5), size = 13, face = "bold", color = "black"),
    axis.title.y = element_text(margin = margin(t = 5), size = 13, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  ylim(0, 7000)

# Print the plot
print(test)


test <- test + stats

test

ggsave("~/Desktop/Arch-Protection_Barplot_stats.jpg", plot = test, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)

