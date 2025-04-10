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

Jurisdiction<- read.csv('/Users/Paolo/Desktop/UHManoa/GRANTS:Scholarships/Sea Grant 2022-2024/Am Sam Dive Trip Plans/FIELDWORK/2024 Survey Feb/2024 Faisua Surveys AS.csv', header = TRUE)
Jurisdiction$Island[Jurisdiction$Island == "Tau"] <- "Ta‘ū"
Jurisdiction$Island[Jurisdiction$Island == "Aunuu"] <- "Aunu‘u"
Jurisdiction$Island[Jurisdiction$Island == "Muliava"] <- "Muliāva"
Jurisdiction$Protection[Jurisdiction$Protection == "Inaccessible (Muliava)"] <- "Inaccessible (Muliāva)"
Jurisdiction$Protection2[Jurisdiction$Protection2 == "Inaccessible (Muliava)"] <- "Inaccessible (Muliāva)"
Jurisdiction[, 3] <- as.character(Jurisdiction[, 3])

#view(Jurisdiction)

unique(Jurisdiction$Protection2)

blue <- rgb(0/255, 114/255, 178/255)
reddish_purple <- rgb(204/255, 121/255, 167/255)
bluish_green <- rgb(0/255, 158/255, 115/255)
sky_blue <- rgb(86/255, 180/255, 233/255)
vermillion <- rgb(213/255, 94/255, 0/255)
yellow <- rgb(240/255, 228/255, 66/255)
black <- rgb(0/255, 0/255, 0/255)

custom_colors <- scale_fill_manual(
  values = c(
    "Federal No Take" = reddish_purple,
    "None" = sky_blue,
    "Remote" = vermillion,
    "Subsistence" = reddish_purple,
    "Subsistence & Remote" = bluish_green,
    "Village Protected" = yellow,
    "Inaccessible (Muliāva)" = black
  )
)



Jurisdiction$Protection2 <- factor(
  Jurisdiction$Protection2,
  levels = c("Federal No Take", "None", "Remote", "Subsistence", "Subsistence & Remote","Village Protected", "Inaccessible (Muliāva)")
)


# Reassign the factor levels for Island in Jurisdiction to ensure the correct order
Jurisdiction$Island <- as.character(Jurisdiction$Island)
Jurisdiction$Island[is.na(Jurisdiction$Island)] <- "Aunu‘u"
unique(Jurisdiction$Island)

# Recalculate the mean and standard error of Clams per Hectare for each Island and Protection group
summary_data <- Jurisdiction %>%
  group_by(Island, Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

summary_data_tutuila <- summary_data %>%
  filter(Island == "Tutuila")

summary_data_island <- Jurisdiction %>%
  group_by(Island) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

summary_data_Protection2 <- Jurisdiction %>%
  group_by(Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

# Ensure that the factor levels in summary_data also follow the same order
#summary_data$Island <- factor(summary_data$Island, levels = c("Tutuila", "Aunu'u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))
unique(Jurisdiction$Island)

Jurisdiction$Island <- factor(Jurisdiction$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))
summary_data$Island <- factor(summary_data$Island, levels = c("Tutuila", "Aunu‘u", "Ofu", "Olosega", "Ta‘ū", "Muliāva"))


# Run a two-way ANOVA to test for differences in clam density between islands and protection status
anova_island_protection <- aov(Clams...Hectare ~ Island * Protection2, data = Jurisdiction)
anova_summary_isl_pro <- summary(anova_island_protection)

anova_island <- aov(Clams...Hectare ~ Island, data = Jurisdiction)
anova_summary_island <- summary(anova_island)

anova_Protection2 <- aov(Clams...Hectare ~  Protection2, data = Jurisdiction)
anova_summary_pro2 <- summary(anova_Protection2)

# Extract the p-value for the interaction between Island and Protection
anova_p_value <- anova_summary[[1]]["Island:Protection2", "Pr(>F)"]

########################        MMA
summary_data_MMA_noMuliava <- Jurisdiction %>%
  filter(Island != "Muliāva") %>%
  group_by(Island, MMA) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )


# Define colorblind-friendly palette
color_palette <- c("#0072B2", "#E69F00")  # Blue and orange

Island_bar_MMA <- ggplot(summary_data_MMA_noMuliava, aes(x = Island, y = mean_clams, fill = MMA)) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.7, preserve = "single"), 
           width = 0.7, 
           color = "black") +
  geom_errorbar(aes(x = as.numeric(Island) + c(-0.16, 0.18, 0, 0, 0, 0),  # Adjust positions here
                    ymin = mean_clams - se_clams, 
                    ymax = mean_clams + se_clams), 
                width = 0.2, 
                position = position_dodge2(width = 0.7, preserve = "single")) +
  scale_fill_manual(values = color_palette, name = "Protection") +
  labs(x = "Island", y = "mean / ha") + 
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
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Display the plot
print(Island_bar_MMA)


ggsave("~/Desktop/Island-Bar2_MMA.jpg", plot = Island_bar_MMA, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)

# Create the bar plot with averages, error bars, and colorblind-friendly colors
Island_bar <- ggplot(summary_data, aes(x = Island, y = mean_clams, fill = Protection2)) + 
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_clams - se_clams, ymax = mean_clams + se_clams), 
                width = 0.2, position = position_dodge(0.8)) + 
  labs(x = "Island", y = "mean / ha", fill = "Protection") + 
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
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Print the plot
print(Island_bar)



####                         SAVE
ggsave("~/Desktop/Island-Protection2_Barplot.jpg", plot = Island_bar, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)


tutuila_data <- Jurisdiction %>% filter(Island == "Tutuila")

ClamsHectare <- tutuila_data$Clams...Hectare
grouped_data <- tutuila_data %>% group_by(Protection2) %>% summarise(n = n())
print(grouped_data)


summary_tutuila_data <- tutuila_data %>%
  group_by(Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()),
    n_clams = n()
  )

# Print summary to ensure it's calculating correctly
print(summary_tutuila_data)

#Mei Lin Neo's population classes
data <- data.frame(
  Class = c("Rare", "Frequent", "Abundant"),
  Density = c(0.1, 10, 100)
)


bar_plot_tut <- ggplot(summary_tutuila_data, aes(x = Protection2, y = mean_clams, fill = Protection2)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 100), fill = "#28ae80", alpha = 0.1) +  # Faint green between 10 and 100
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.1, ymax = 10), fill = "darkred", alpha = 0.1) +  # Faint red below 10
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_clams - se_clams, ymax = mean_clams + se_clams), 
                width = 0.2, position = position_dodge(0.8)) + 
  labs(
    x = "Tutuila", 
    y = "mean / ha", 
    fill = "Protection"  # Set the legend title to "Protection"
  ) +
  custom_colors +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10, face = "bold", color = "black"),
    legend.title = element_text(size = 13, face = "bold", color = "black"),
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(margin = margin(t = 5), size = 13, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(t = 5), size = 13, color = "black", face = "bold"),
    axis.text.x = element_text(size = 9, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  geom_hline(yintercept = 0.1, linetype = "dotted", color = "#210c4a", size = 1) +
  geom_hline(yintercept = 10, linetype = "solid", color = "darkred", size = 1) +
  geom_hline(yintercept = 100, linetype = "twodash", color = "#28ae80", size = 1) +
  geom_text(aes(label = n_clams, y = mean_clams + se_clams + 10), 
            position = position_dodge(0.8), vjust = 0.5, size = 4, fontface = "bold", color = "black")

# Display the plot
print(bar_plot_tut)

get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Modify the axis labels to wrap if they are too long
# bar_plot2 is a modified version of bar_plot for the combined plot
Island_bar3 <- Island_bar + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Wrap labels to 10 characters
  labs(x = "Island", y = "mean / ha", size = 13, face = "bold") + 
  custom_colors + 
  theme(
    plot.margin = margin(5, 5, 5, 5),  # Set equal margins
    axis.title.y = element_text(margin = margin(t = 5, b = 5))
    # Set y-axis title with padding
  )

########################################################################################################
# ANOVA for Island
aov_island <- aov(Clams...Hectare ~ Island, data = Jurisdiction)
sum_island <- summary(aov_island)
print("ANOVA results for Island:")
print(sum_island)

# Extract p-value for Island
p_value_island <- sum_island[[1]]$`Pr(>F)`[1]
print(paste("P-value for Island:", p_value_island))

# ANOVA for Protection2
aov_protection <- aov(Clams...Hectare ~ Protection2, data = Jurisdiction)
sum_protection <- summary(aov_protection)
print("ANOVA results for Protection2:")
print(sum_protection)

# Extract p-value for Protection2
p_value_protection <- sum_protection[[1]]$`Pr(>F)`[1]
print(paste("P-value for Protection2:", p_value_protection))

# Post-hoc Tukey HSD for Island
tukey_island <- TukeyHSD(aov_island)
print("Tukey HSD results for Island:")
print(tukey_island)

# Post-hoc Tukey HSD for Protection2
tukey_protection <- TukeyHSD(aov_protection)
print("Tukey HSD results for Protection2:")
print(tukey_protection)


########################################################################################################
bar_plot_tut2 <- ggplot(summary_tutuila_data, aes(x = Protection2, y = mean_clams, fill = Protection2)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 100), fill = "#28ae80", alpha = 0.1) +  # Faint green between 10 and 100
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.1, ymax = 10), fill = "red", alpha = 0.1) +  # Faint red below 10
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_clams - se_clams, ymax = mean_clams + se_clams), 
                width = 0.2, position = position_dodge(0.8)) + 
  labs(x = "Tutuila", y = "mean / ha", size = 13, face = "bold") + 
  custom_colors +
  theme_minimal() + 
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10, face = "bold"),   # Increase size and bold legend text
    legend.title = element_text(size = 13, face = "bold"),  # Increase size and bold legend title
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),  # Y-axis title
    axis.text.y = element_text(size = 8, face = "bold", color = "black"),  # Y-axis text style
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),  # X-axis title style
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(5, 5, 5, 5)  # Set equal margins
  ) + 
  scale_y_continuous(
    limits = c(-10, max(summary_tutuila_data$mean_clams + summary_tutuila_data$se_clams + 20, na.rm = TRUE)), 
    breaks = c(0, 10, 50, 100, 200),  # Explicitly set labeled ticks
    labels = function(x) {
      ifelse(x == 0, paste0(" ", x), x)  # Add a space before the "0" label
    }
  ) +
  geom_text(aes(label = n_clams, y = mean_clams + se_clams + 5),  # Lowered the numbers by reducing from +10 to +5
            position = position_dodge(0.5), vjust = c(-1.2, -0.5, -0.5, -0.5, -0.5) , size = 4, fontface = "bold", color = "black") +
  # Overlay horizontal lines for different population classes
  geom_hline(yintercept = 0.1, linetype = "dotted", color = "#210c4a", size = 1) +  # Rare in first viridis color
  geom_hline(yintercept = 10, linetype = "solid", color = "#860808", size = 1) +    # Frequent in second viridis color
  geom_hline(yintercept = 100, linetype = "twodash", color = "#28ae80", size = 1)   # Abundant in third viridis color

# Print the plot
print(bar_plot_tut2)


#################################################################################################
# Replace 0 with a small value in the data
summary_tutuila_data <- summary_tutuila_data %>%
  mutate(mean_clams = ifelse(mean_clams == 0, 0.01, mean_clams))  # Add small value to zero entries


######################################################################################

bar_plot_tut2
shared_legend <- get_legend(Island_bar3) # Extract the legend from bar_plot2

Island_bar3 <- Island_bar3 + theme(legend.position = "none")
bar_plot_tut2 <- bar_plot_tut2 + theme(legend.position = "none")

grid.arrange(
  arrangeGrob(Island_bar3, bar_plot_tut2, ncol = 2, widths = c(0.65, 0.35)),# heights = c(4, 4)),  # Arrange plots side by side
  shared_legend,  # Add the shared legend below the plots
  nrow = 2,  # 2 rows: plots on top, legend below
  heights = c(10, 1)  # Make the plots taller and the legend smaller
)



{ jpeg("~/Desktop/combined_plot_protect2.jpeg", width = 12, height = 8, units = "in", res = 300)  # Customize dimensions and resolution

# Combine the plots and save to JPEG
grid.arrange(
  arrangeGrob(Island_bar3, bar_plot_tut2, ncol = 2, widths = c(0.65, 0.35)),  # Arrange the plots side by side
  shared_legend,  # Add the shared legend below
  nrow = 2,  # Organize in two rows: plots on top, legend below
  heights = c(10, 1)  # Make the plots taller and the legend smaller
)

# Close the JPEG device
dev.off()
}

clamsize<- read.csv('/Users/Paolo/Desktop/Faisua/Data (Besides Photos)/Am Samoa Clams Alison Green/2022-2024_All_Clams_Measured.csv', header = TRUE)
clamssize_tut <- clamsize %>% filter(Island == c("Tutuila"))
clamssize_tut_aunuu <- clamsize %>% filter(Island == c("Tutuila", "Aunu‘u"))


{ qq <- ggplot(clamssize_tut_aunuu, aes(x = Protection2, y = Size, fill = Protection2)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  labs(x = "Tutuila") + 
  custom_colors +
  labs(y = "Clam Size (cm)") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10, face = "bold"),   # Increase size and bold legend text
    legend.title = element_text(size = 13, face = "bold"),  # Increase size and bold legend title
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.y = element_text( size = 13, face = "bold", color = "black"), 
    axis.text.y = element_text(size = 13, face = "bold", color = "black"),  # Adjust x-axis title style
    axis.text.x = element_blank(),   # Remove x-axis labels
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),  # Adjust x-axis title style
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    #plot.margin = margin(5, 5, 5, 5)  # Set equal margins
  )}
qq

valid_comparisons <- list(c("None", "Federal No Take"),
                          c("None", "Remote"),
                          c("None", "Village Protected"),
                          c("Federal No Take", "Remote"),
                          c("Federal No Take", "Village Protected"),
                          c("Remote", "Village Protected"))  


  # Add statistical comparisons (ANOVA by default, p-value displayed)
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
                     label.y = c(21, 23, 25, 27, 29, 31))
)

stats2 <- list(
  stat_compare_means(label = "p.signif", label.y = 19,
                     method = "anova"),  
  stat_compare_means(comparisons = list(c("None", "Remote"),
                                        c("None", "Village Protected"),
                                        c("None", "Subsistence & Remote")
                                        c("Remote", "Subsistence & Remote"),
                                        c("Subsistence & Remote", "Village Protected")
                                        c("Remote", "Village Protected")),
                     method = "t.test", 
                     label = "p.signif", 
                     label.y = c(25, 28, 31))
)

stats2

summary_tutuila_data <- tutuila_data %>%
  group_by(Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()),
    n_clams = n()
  )


pp <- ggplot(clamssize_tut, aes(x = Protection2, y = Size, fill = Protection2)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  labs(x = "Tutuila") + 
  custom_colors +
  labs(y = "Clam Size (cm)") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    legend.text = element_text(size = 10, face = "bold"),   
    legend.title = element_text(size = 13, face = "bold"),  
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"), 
    axis.text.y = element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_blank(),   
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  scale_y_continuous(position = "right") +
  
  # Add sample sizes below each boxplot at y = 2
  geom_text(data = summary_tutuila_data, 
            aes(x = Protection2, y = 2, label = paste0("n = ", n_clams)), 
            inherit.aes = FALSE, size = 4, fontface = "bold")

pp

qq_with_stats <- qq + stats
pp_with_stats <- pp + stats2

ggsave("~/Desktop/pp_with_stats.jpg", plot = pp_with_stats, device = "jpeg", width = 8, height = 10, units = "in", dpi = 300)


#########################################            ARCHIPELAGO WIDE

anova_result_noIsl <- aov(Clams...Hectare ~ Protection2, data = Jurisdiction)
anova_summary_noIsl <- summary(anova_result_noIsl)
anova_p_value_noIsl <- anova_summary_noIsl[[1]]["Protection2", "Pr(>F)"]

summary_data_noIsl <- Jurisdiction %>%
  group_by(Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

Protection2_bar <- ggplot(summary_data_noIsl, aes(x = Protection2, y = mean_clams, fill = Protection2)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_clams - se_clams, ymax = mean_clams + se_clams), 
                width = 0.2, position = position_dodge(0.8)) + 
  geom_text(aes(label = paste("n =", n_clams)), 
            vjust = -5, size = 4, fontface = "bold") +
  labs(x = "Island", y = "mean / ha", color = "black") + 
  custom_colors +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9, face = "bold", color = "black"),
    legend.title = element_text(size = 13, face = "bold", color = "black"),  # Increase size and bold legend title
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    axis.title.x = element_text(margin = margin(t = 5), size = 13, face = "bold", color = "black"),
    axis.title.y = element_text(margin = margin(t = 5), size = 13, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1))

# Print the plot
print(Protection2_bar)

#ggsave("~/Desktop/Arch-Protection2_Barplot.jpg", plot = Protection2_bar, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)

#########################################            STATS


bar_plot_tut2_stats <- bar_plot_tut2 + 
  stat_compare_means(label = "p.signif", label.y = 22, method = "anova") +  # Overall ANOVA
  stat_compare_means(comparisons = valid_comparisons, 
                     method = "t.test", 
                     label = "p.signif", 
                     label.y = c(500, 700, 900, 1100, 1300, 1500))



Island_bar3_labeled <- Island_bar3 + annotation_custom(textGrob("a)", x = unit(0.02, "npc"), y = unit(0.98, "npc"), 
              just = c("left", "top"), gp = gpar(fontsize = 14, fontface = "bold")))

bar_plot_tut2_labeled <- bar_plot_tut2 + annotation_custom(textGrob("b)", x = unit(0.02, "npc"), y = unit(0.98, "npc"), 
              just = c("left", "top"), gp = gpar(fontsize = 14, fontface = "bold")))

pp_with_stats_labeled <- pp_with_stats + annotation_custom(textGrob("c)", x = unit(0.02, "npc"), y = unit(0.98, "npc"), 
              just = c("left", "top"), gp = gpar(fontsize = 14, fontface = "bold")))

TEST2_with_stats_labeled <- TEST2_with_stats + annotation_custom(textGrob("c)", x = unit(0.02, "npc"), y = unit(0.98, "npc"), 
              just = c("left", "top"), gp = gpar(fontsize = 14, fontface = "bold")))


#########################################            GRID ARRANGE

jpeg("~/Desktop/Protection2_combined_plot2.jpeg", width = 12, height = 8, units = "in", res = 300)  # Customize dimensions and resolution
# Arrange the plots
grid.arrange(
  Island_bar3_labeled, # First row: Island_bar2 with label
  arrangeGrob(bar_plot_tut2_labeled, TEST2_with_stats_labeled, ncol = 2, widths = c(0.5, 0.5)), # Second row: bar_plot_tut2 and pp with labels
  shared_legend, # Shared legend at the bottom
  nrow = 3,  # Organize in 3 rows
  heights = c(5, 3, 0.7)  # Adjust the row heights
)

 # Close the JPEG device
dev.off()



###############
