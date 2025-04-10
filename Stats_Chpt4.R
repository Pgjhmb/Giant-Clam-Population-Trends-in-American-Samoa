
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

Tutuila <- subset(Jurisdiction, Island == "Tutuila")

########################################################################################################

summary_island_Pro2 <- Jurisdiction %>%
  group_by(Island, Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

summary_tutuila <- summary_island_Pro2 %>%
  filter(Island == "Tutuila")
summary_tutuila

summary_island <- Jurisdiction %>%
  group_by(Island) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

summary_island_MMA <- Jurisdiction %>%
  group_by(MMA) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

summary_Protection2 <- Jurisdiction %>%
  group_by(Protection2) %>%
  summarise(
    mean_clams = mean(Clams...Hectare, na.rm = TRUE),
    se_clams = sd(Clams...Hectare, na.rm = TRUE) / sqrt(n()), # Standard Error
    n_clams = n()
  )

########################################################################################################
Tutuila <- subset(Jurisdiction, Island == "Tutuila")

Tutuila_Aunuu <- subset(Jurisdiction, Island %in% c("Tutuila", "Aunuu"))

# Run ANOVA for Clams per Hectare within Tutuila
aov_tutuila <- aov(Clams...Hectare ~ Protection2, data = Tutuila)
shapiro.test(residuals(aov_tutuila))

# Summarize the ANOVA results
sum_tutuila <- summary(aov_tutuila)

# Print results
print("ANOVA results for Tutuila:")
print(sum_tutuila)
#############

aov_tutuila_aunuu <- aov(Clams...Hectare ~ Protection2, data = Tutuila_Aunuu)

# Summarize the ANOVA results
sum_tutuila_aunuu <- summary(aov_tutuila_aunuu)

# Print results
print("ANOVA results for Tutuila:")
print(sum_tutuila_aunuu)


########################
# Run ANOVA for Clams per Hectare within Tutuila
aov_MMA <- aov(Clams...Hectare ~ MMA, data = Jurisdiction)
tukey_MMA <- TukeyHSD(aov_MMA)

# Summarize the ANOVA results
aov_tutuila_MMA <- aov(Clams...Hectare ~ MMA, data = Tutuila)
sum_MMA <- summary(aov_tutuila_MMA)

########################

# Run ANOVA for Clams per Hectare within Tutuila
aov_tutuila_quadrant <- aov(Clams...Hectare ~ Quadrant, data = Tutuila)

# Summarize the ANOVA results
sum_tutuila_quadrant <- summary(aov_tutuila_quadrant)

# Print results
print("ANOVA results for Tutuila:")
print(sum_tutuila_quadrant)


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
aov_protection2 <- aov(Clams...Hectare ~ Protection2, data = Jurisdiction)
sum_protection2 <- summary(aov_protection2)
print("ANOVA results for Protection2:")
print(sum_protection2)

# Extract p-value for Protection2
p_value_protection2 <- sum_protection2[[1]]$`Pr(>F)`[1]
print(paste("P-value for Protection2:", p_value_protection2))

tukey_protection2 <- TukeyHSD(aov_protection2)

# ANOVA for Protection
aov_protection <- aov(Clams...Hectare ~ Protection, data = Jurisdiction)
sum_protection <- summary(aov_protection)
print("ANOVA results for Protection2:")
print(sum_protection)

# Extract p-value for Protection2
p_value_protection <- sum_protection[[1]]$`Pr(>F)`[1]
print(paste("P-value for Protection2:", p_value_protection))
tukey_protection <- TukeyHSD(aov_protection)


# Post-hoc Tukey HSD for Island
tukey_island <- TukeyHSD(aov_island)
print("Tukey HSD results for Island:")
print(tukey_island)

# Post-hoc Tukey HSD for Protection2
tukey_protection <- TukeyHSD(aov_protection)
print("Tukey HSD results for Protection2:")
print(tukey_protection)

########################################################################################################

