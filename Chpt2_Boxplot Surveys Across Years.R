rm(list=ls()) # clean up
if (!is.null(dev.list())) {
  dev.off()  # Close the current active device
  while (!is.null(dev.list())) {
    dev.off()  # Keep closing until all devices are closed
  }
}
# Load necessary libraries
library(dplyr)
library(lmtest)
library(tidyverse)
library(mvabund)
library(hrbrthemes)
library(viridis)
library(viridisLite)
library(ggplot2)
library(multcompView)
library(ggsignif)
library(ggpubr)
library(lme4)
library(Matrix)
library(emmeans)
library(hrbrthemes)
library(gridExtra)
library(grid)
# Load the data
SurveyData <- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2024 Survey Feb/1996-24 All Surveys.csv')


# Load necessary libraries
#view(SurveyData)

SurveyData <- SurveyData %>%
  mutate(Island = as.character(Island)) %>% 
  mutate(Island = recode(Island, 
                         "Tau" = "Ta‘ū", 
                         "Aunuu" = "Aunu‘u"))

# Convert back to a factor with the correct order
SurveyData$Island <- factor(SurveyData$Island, 
                            levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū"))
# Calculate mean and standard error for Clams / Hectare by Island and Year
Dens <- SurveyData$Mean.Dens
SE <- SurveyData$SE
Num <- SurveyData$Mean.Num
Year <- SurveyData$Year
Island <- SurveyData$Island

#library(grid)   
# For rasterGrob to add the image
#library(png)        # For reading PNG images
#library(cowplot)    # Optional, for `background_image()`

SurveyData
#SurveyData$Island <- factor(SurveyData$Island, levels = c("Tutuila","Aunu'u", "Ofu", "Olosega", "Ta‘ū"))
SurveyData$MeanDens <- SurveyData$Mean.Dens

anova_island_Dens <- aov(Mean.Dens ~ Island, data = SurveyData)
summary(anova_island_Dens) #       0.00466 **

anova_island_year <- aov(Mean.Dens ~ Year, data = SurveyData)
summary(anova_island_year) #       0.392

Island_years_Boxplot <- ggplot(SurveyData, aes(x = Island, y = Mean.Dens, fill = Year)) + 
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Dens - SE, ymax = Dens + SE), 
                width = 0.2, position = position_dodge(0.8)) + 
  scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1250, by = 250)) + 
  scale_fill_viridis_d(name = "Year") + 
  labs(x = "Island", y = "Mean Density (clams / ha)") +
  theme_minimal() +   
  theme(
    legend.position = c(0.18, 0.953),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid"),
    legend.text = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 14, face = "bold", color = "black"),
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(margin = margin(t = 10), size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(margin = margin(t = 10), size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Print the plot
print(Island_years_Boxplot)


ggsave("~/Desktop/Island-years_Boxplot.jpg", plot = Island-years_Boxplot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)


