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
library(multcompView)
library(ggsignif)
library(ggpubr)
library(readxl)
# Replace with your actual file path
clamsize<- read_excel('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2022-2024_All_Clams_Measured.xlsx', sheet = "2022-2024_All_Clams_Measured")
#clamsize<- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2022-2024_All_Clams_Measured.csv', header = TRUE)
attach(clamsize)

view(clamsize)

clamsize$Island[clamsize$Island == "Rose"] <- "Muliāva"
clamsize$Island[clamsize$Island == "Muliava"] <- "Muliāva"
clamsize$Protection2[clamsize$Protection2 == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize$Protection[clamsize$Protection == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize$PROTECTION[clamsize$PROTECTION == "Federal No Take & Remote"] <- "Inaccessible (Muliāva)"

unique(clamsize$PROTECTION)
unique(clamsize$Protection2)

#desire_order <- c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Rose"))
clamsize$Island <- factor(clamsize$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))
# Sort data based on the custom order of Island
clamsize <- clamsize %>%
  arrange(Island)

# Example box plot
q <- ggplot(clamsize, aes(x = Island, y = Size)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  labs(title = "Box-and-Whisker Plot",
       x = "Island",  # Fixed the label for x-axis to match Island
       y = "Clam Size (cm)")  # Fixed the label for y-axis to match Size
q
##

p <- ggplot(clamsize, aes(x=Island, y=Size)) + 
  geom_violin(width=1.1) +
    aes(fill = Island)+
  geom_boxplot(width=0.1, color="grey", alpha=0.2,
      outlier.colour="darkred", outlier.shape=9,outlier.size=6) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Island",
      y = "Clam Size (cm)")
p



unique(clamsize$Island)
# Standardize Island levels
clamsize$Island <- factor(clamsize$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))


data_counts <- clamsize %>%
  group_by(Island, Protection2) %>%
  summarise(n = n(), mean_size = mean(Size))

data_counts2 <- clamsize %>%
  group_by(Island) %>%
  summarise(n = n(), mean_size = mean(Size))
            

tb <- ggplot(clamsize, aes(x = Island, y = Size, fill = Island)) +
  geom_violin(width = 1.1, color = "black") + 
  geom_boxplot(width = 0.2, fill = "lightgrey", color = "black", alpha = 0.9, lwd = 0.6,
               outlier.colour = "purple", outlier.shape = 20, outlier.size = 5) + 
  geom_area(data = clamsize, aes(x = Island, y = Density_Islan / 35, group = 1), 
            fill = viridis(1, option = "D"), color = viridis(1, option = "D"), 
            alpha = 0.2, size = 1) +  # Adjust y scaling for area plot
  scale_fill_viridis(discrete = TRUE) + 
  
  labs(x = "Island", y = "Clam Size (cm)") +
  guides(fill = "none") +  # Removes the legend for fill
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"), # Center and bold the title
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "black"), # Bold x-axis label
    axis.title.y = element_text(face = "bold", color = "black"), # Bold primary y-axis label
    axis.title.y.right = element_text(face = "bold", color = "black"), # Bold secondary y-axis label
    axis.text.x = element_text(face = "bold", color = "black"),  # Bold x-axis labels (island names)
    axis.text.y = element_text(face = "bold", color = "black"),  # Bold y-axis labels
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "grey80"),  # Major grid lines
    panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
  ) +
  
  # Add the number of observations above each violin plot
  geom_text(data = data_counts, aes(x = Island, y = 34, label = paste("n =", n)),
            vjust = -1, size = 4, fontface = "bold") +  # Adjust the y position and size as needed
  
  # Define primary y-axis range and secondary y-axis
  scale_y_continuous(
    limits = c(0, 35),  # Primary y-axis range for clam size
    sec.axis = sec_axis(~ . * 35, name = expression(bold("Density (number ha"^-1*")")), breaks = seq(0, 1200, 200))  # Secondary axis for density
  )

tb

#ggsave("~/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Working R Scripts/ClamSize_Island_BoxWhisker.jpg", plot = tb, device = "jpg", width = 8, height = 6, units = "in", dpi = 300)
#############
clamsize_94_24<- read_excel('/Users/Paolo/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Chp_4- Population Trends/All_Surveys_PMB-4-15.xlsx', sheet = "Rawdata_all")
clamsize_94_24_Rose<- read_excel('/Users/Paolo/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Chp_4- Population Trends/All_Surveys_PMB-4-15.xlsx', sheet = "Rawdata_ALL_Rose")
#clamsize<- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2022-2024_All_Clams_Measured.csv', header = TRUE)
attach(clamsize_94_24)

#view(clamsize_94_24)

data_counts_94_24 <- clamsize_94_24 %>%
  group_by(Island) %>%
  summarise(n = n())

clamsize_94_24$Island[clamsize_94_24$Island == "Rose"] <- "Muliāva"
clamsize_94_24$Island[clamsize_94_24$Island == "Muliava"] <- "Muliāva"
clamsize_94_24$Protection2[clamsize_94_24$Protection2 == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize_94_24$Protection[clamsize_94_24$Protection == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize_94_24$PROTECTION[clamsize_94_24$PROTECTION == "Federal No Take & Remote"] <- "Inaccessible (Muliāva)"

unique(clamsize_94_24$Island)

#desire_order <- c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Rose"))
clamsize_94_24$Island <- factor(clamsize_94_24$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))
# Sort data based on the custom order of Island
clamsize_94_24 <- clamsize_94_24 %>%
  arrange(Island)

tc <- ggplot(clamsize_94_24, aes(x = Island, y = Size, fill = Island)) +
  geom_violin(width = 1.1, color = "black") + 
  geom_boxplot(width = 0.2, fill = "lightgrey", color = "black", alpha = 0.9, lwd = 0.6,
               outlier.colour = "purple", outlier.shape = 20, outlier.size = 5) + 
  geom_area(data = clamsize, aes(x = Island, y = Density_Islan / 35, group = 1), 
            fill = viridis(1, option = "D"), color = viridis(1, option = "D"), 
            alpha = 0.2, size = 1) +  # Adjust y scaling for area plot
  scale_fill_viridis(discrete = TRUE) + 
  
  labs(x = "Island", y = "Clam Size (cm)") +
  guides(fill = "none") +  # Removes the legend for fill
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"), # Center and bold the title
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "black"), # Bold x-axis label
    axis.title.y = element_text(face = "bold", color = "black"), # Bold primary y-axis label
    axis.title.y.right = element_text(face = "bold", color = "black"), # Bold secondary y-axis label
    axis.text.x = element_text(face = "bold", color = "black"),  # Bold x-axis labels (island names)
    axis.text.y = element_text(face = "bold", color = "black"),  # Bold y-axis labels
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "grey80"),  # Major grid lines
    panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
  ) +
  
  # Add the number of observations above each violin plot
  geom_text(data = data_counts_94_24, aes(x = Island, y = 34, label = paste("n =", n)),
            vjust = -2, size = 4, fontface = "bold") +  # Adjust the y position and size as needed
  
  # Define primary y-axis range and secondary y-axis
  scale_y_continuous(
    limits = c(0, 38),  # Primary y-axis range for clam size
    sec.axis = sec_axis(~ . * 34, name = expression(bold("Density (number ha"^-1*")")), breaks = seq(0, 1200, 200))  # Secondary axis for density
  )

tc

data_counts_94_24 <- clamsize_94_24 %>%
  group_by(Island, Protection2) %>%
  summarise(n = n(), mean_size = mean(Size))

data_counts_94_24_2 <- clamsize_94_24 %>%
  group_by(Island) %>%
  summarise(n = n(), mean_size = mean(Size))


ggsave("~/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Working R Scripts/ClamSize94-24_Island_BoxWhisker.jpg", plot = tc, device = "jpg", width = 8, height = 6, units = "in", dpi = 300)

#########

data_counts_94_24_Rose <- clamsize_94_24_Rose %>%
  group_by(Island) %>%
  summarise(n = n())

clamsize_94_24_Rose$Island[clamsize_94_24_Rose$Island == "Rose"] <- "Muliāva"
clamsize_94_24_Rose$Island[clamsize_94_24_Rose$Island == "Muliava"] <- "Muliāva"
clamsize_94_24_Rose$Protection2[clamsize_94_24_Rose$Protection2 == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize_94_24_Rose$Protection[clamsize_94_24_Rose$Protection == "Inaccessible (Rose Atoll)"] <- "Inaccessible (Muliāva)"
clamsize_94_24_Rose$PROTECTION[clamsize_94_24_Rose$PROTECTION == "Federal No Take & Remote"] <- "Inaccessible (Muliāva)"

unique(clamsize_94_24_Rose$Island)

#desire_order <- c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Rose"))
clamsize_94_24_Rose$Island <- factor(clamsize_94_24_Rose$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))
# Sort data based on the custom order of Island
clamsize_94_24_Rose <- clamsize_94_24_Rose %>%
  arrange(Island)

td <- ggplot(clamsize_94_24_Rose, aes(x = Island, y = Size, fill = Island)) +
  geom_violin(width = 1.1, color = "black") + 
  geom_boxplot(width = 0.2, fill = "lightgrey", color = "black", alpha = 0.9, lwd = 0.6,
               outlier.colour = "purple", outlier.shape = 20, outlier.size = 5) + 
  geom_area(data = clamsize, aes(x = Island, y = Density_Islan / 35, group = 1), 
            fill = viridis(1, option = "D"), color = viridis(1, option = "D"), 
            alpha = 0.2, size = 1) +  # Adjust y scaling for area plot
  scale_fill_viridis(discrete = TRUE) + 
  
  labs(x = "Island", y = "Clam Size (cm)") +
  guides(fill = "none") +  # Removes the legend for fill
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"), # Center and bold the title
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", color = "black"), # Bold x-axis label
    axis.title.y = element_text(face = "bold", color = "black"), # Bold primary y-axis label
    axis.title.y.right = element_text(face = "bold", color = "black"), # Bold secondary y-axis label
    axis.text.x = element_text(face = "bold", color = "black"),  # Bold x-axis labels (island names)
    axis.text.y = element_text(face = "bold", color = "black"),  # Bold y-axis labels
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = "grey80"),  # Major grid lines
    panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
  ) +
  
  # Add the number of observations above each violin plot
  geom_text(data = data_counts_94_24_Rose, aes(x = Island, y = 34, label = paste("n =", n)),
            vjust = -2, size = 4, fontface = "bold") +  # Adjust the y position and size as needed
  
  # Define primary y-axis range and secondary y-axis
  scale_y_continuous(
    limits = c(0, 38),  # Primary y-axis range for clam size
    sec.axis = sec_axis(~ . * 34, name = expression(bold("Density (number ha"^-1*")")), breaks = seq(0, 1200, 200))  # Secondary axis for density
  )

td

all_size_summary <- clamsize_94_24_Rose %>%
  group_by(Island, Year) %>%
  summarise(Mean = mean(Size, na.rm = TRUE),
            SE = sd(Size, na.rm = TRUE) / sqrt(n()), .groups = "drop")
all_size_summary

ggsave("~/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Working R Scripts/ClamSize94-24_Island_ROSE_BoxWhisker.jpg", plot = td, device = "jpg", width = 8, height = 6, units = "in", dpi = 300)
###

Roseclamsize<- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2017 NPSA Data/2017-22 Rose Survey Data.csv', header = TRUE)
#view(Roseclamsize)

#view(clamsize)
# ANOVA for mean giant clam size across years
anova_Protection2 <- aov(Size ~ Protection2, data = clamsize)
summary(anova_Protection2) #        9.3e-13

anova_island <- aov(Size ~ Island, data = clamsize)
summary(anova_island) #       5.36 e -13
tukey_result <- TukeyHSD(anova_island)

Roseclams <- Roseclamsize$Size

# Perform ANOVA
anova_result <- aov(Size ~ Site, data = Roseclamsize)

# Perform Tukey's Honest Significant Difference (HSD) test
tukey_result <- TukeyHSD(anova_result)

# Extract Tukey HSD results and create groupings
tukey_groups <- multcompView::multcompLetters4(anova_result, tukey_result)$Site

# Convert the groupings to a data frame
tukey_labels <- as.data.frame.list(tukey_groups)
tukey_labels$Site <- rownames(tukey_labels)
colnames(tukey_labels)[1] <- "Letters"  # Ensure the column for labels is named "Letters"

# Boxplot with significance letters (a, b, c)
boxplot_gg <- ggplot(Roseclamsize, aes(x = Site, y = Size, fill = Site)) +  # Use full data frame here
  geom_boxplot() +
  labs(title = "Boxplot of Clam Sizes Across Sites", x = "Site", y = "Clam Size (cm)") +
  theme_minimal() +
  geom_text(data = tukey_labels, aes(x = Site, y = max(Roseclamsize$Size) + 2, label = Letters), size = 6)  # Dynamic placement of labels above boxes

# Display the plot
print(boxplot_gg)

# Boxplot

boxplot_gg_title <- ggplot(Roseclamsize, aes(x = Site, y = Size, fill = Site)) +
  geom_boxplot() +
  labs(title = "Rose Atoll Clam Sizes Across Quadrants", x = "Site", y = "Clam Size (cm)") +
  theme_minimal()

# Define all pairwise comparisons between sites
comparisons <- list(c("NE", "NW"), c("NE", "SE"), c("NE", "SW"), 
                    c("NW", "SE"), c("NW", "SW"), c("SE", "SW"))

# Boxplot with pairwise p-values between all sites
boxplot_gg_WEcompare <- ggplot(Roseclamsize, aes(x = Site, y = Size, fill = Site)) +
  geom_boxplot() +
  labs(title = "Rose Atoll Clam Sizes Across Sites", x = "Site", y = "Clam Size (cm)") +
  theme_minimal() +
  stat_compare_means(comparisons = list(c("NE", "NW"), c("SE", "SW")), label = "p.signif") +  # Pairwise comparisons
  stat_compare_means(method = "anova", label.y = 30, size = 5, vjust = -0.5, hjust = -1.2) +  # Centered ANOVA text
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold the title
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 14),  # Center and bold x-axis title
    axis.title.y = element_text(hjust = 0.5, face = "bold", size = 14),  # Center and bold y-axis title
    axis.text = element_text(size = 12)  # Adjust axis text size
  )

# Display the plot
boxplot_gg_WEcompare

# Violin Plot
# # Boxplot with p-values using ggpubr
# violin_gg_allcompare <- ggplot(Roseclamsize, aes(x = Site, y = Size, fill = Site)) +
#   geom_violin() +
#   labs(title = "", x = "Site", y = "Clam Size") +
#   theme_minimal() 
#   #stat_compare_means(comparisons = comparisons, label = "p.signif") +  # Pairwise comparisons
#   #stat_compare_means(method = "anova", label.y = 30)  # Global ANOVA p-value
# 
# violin_gg_allcompare



# Dot Plot
# dotplot_gg <- ggplot(Roseclamsize, aes(x = Site, y = Size, color = Site)) +
#  geom_jitter(width = 0.1, size = 2) +
#  labs(title = "", x = "Site", y = "Clam Size") +
#  theme_minimal()
# 
# dotplot_gg

# violin_gg <- ggplot(Roseclamsize, aes(x = Site, y = Size, fill = Site)) +
#   geom_violin() +
#   labs(title = "", x = "Site", y = "Clam Size") +
#   theme_minimal()
#   #stat_compare_means(comparisons = list(c("NE", "NW"), c("SE", "SW")), label = "p.signif") +
#   #stat_compare_means(method = "anova", label.y = 30)
# violin_gg

# # Histogram Facetted by Site
# histogram_gg <- ggplot(Roseclamsize, aes(x = Size, fill = Site)) +
#   geom_histogram(binwidth = 2, color = "black") +
#   labs(title = "", x = "Clam Size", y = "Count") +
#   facet_wrap(~ Site) +
#   theme_minimal()
# 
# histogram_gg 

# Display all plots together
library(gridExtra)
allplot <- c(boxplot_gg, violin_gg, dotplot_gg, histogram_gg, nrow = 2)
allplot <-  grid.arrange(boxplot_gg, violin_gg, dotplot_gg, histogram_gg, nrow = 2)
allplot
ggsave("~/Desktop/Rose_clam_size_quadrant.jpg", plot = allplot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)

ggsave("~/Desktop/Rose_clam_size_quadrant_violin.jpg", plot = violin_gg_title, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)
ggsave("~/Desktop/Rose_clam_size_quadrant_boxplot.jpg", plot = boxplot_gg_title, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)
ggsave("~/Desktop/Rose_clam_size_quadrant_boxplot.jpg", plot = boxplot_gg_WEcompare, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)
