
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
library(ggpubr)
library(readr)
library(readxl)
library(broom)  # for tidy p-values

ReefSlope_allyears <- read_excel('/Users/Paolo/Desktop/Faisua/Projects/Faisua_Genetics/Thesis_Chapters/Dissertation/Chp_4- Population Trends/All_Surveys_PMB-4-15.xlsx', sheet = "Vert_All_yrs")
ReefSlope_allyears <- ReefSlope_allyears %>%
  filter(Habitat == "Reef Slope")


# Clean and prep
ReefSlope_allyears <- ReefSlope_allyears %>%
  filter(!is.na(Dens), !is.nan(Dens), is.finite(Dens), Dens != "-") %>%
  mutate(
    Dens = as.numeric(Dens),
    Year = factor(Year),
    Island = factor(Island),
    Protection = factor(Protection)
  )

# ---------- ANOVAs ----------

# 1. Year
aov_year <- aov(Dens ~ Year, data = ReefSlope_allyears)
p_year <- tidy(aov_year)$p.value[1]
cat("1. ANOVA: Year\n")
print(summary(aov_year))
eta_squared(aov_year, partial = FALSE)  # η² (general effect size)
eta_squared(aov_year, partial = TRUE)   # η²ₚ (partial effect size)
if (p_year < 0.05) print(TukeyHSD(aov_year))

# 2. Island
aov_island <- aov(Dens ~ Island, data = ReefSlope_allyears)
p_island <- tidy(aov_island)$p.value[1]
cat("\n2. ANOVA: Island\n")
print(summary(aov_island))
eta_squared(aov_island, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island, partial = TRUE)   # η²ₚ (partial effect size)
if (p_island < 0.05) print(TukeyHSD(aov_island))

# 3. Protection
aov_protection <- aov(Dens ~ Protection, data = ReefSlope_allyears)
p_protection <- tidy(aov_protection)$p.value[1]
cat("\n3. ANOVA: Protection\n")
print(summary(aov_protection))
eta_squared(aov_protection, partial = FALSE)  # η² (general effect size)
eta_squared(aov_protection, partial = TRUE)   # η²ₚ (partial effect size)
if (p_protection < 0.05) print(TukeyHSD(aov_protection))

# 4. Island * Year
aov_island_year <- aov(Dens ~ Island * Year, data = ReefSlope_allyears)
cat("\n4. ANOVA: Island * Year\n")
print(summary(aov_island_year))
eta_squared(aov_island_year, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island_year, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_island_year))

# 5. Protection * Year
aov_prot_year <- aov(Dens ~ Protection * Year, data = ReefSlope_allyears)
cat("\n5. ANOVA: Protection * Year\n")
print(summary(aov_prot_year))
eta_squared(aov_prot_year, partial = FALSE)  # η² (general effect size)
eta_squared(aov_prot_year, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_prot_year))

# 6. Island * Protection
aov_island_prot <- aov(Dens ~ Island * Protection, data = ReefSlope_allyears)
cat("\n6. ANOVA: Island * Protection\n")
print(summary(aov_island_prot))
eta_squared(aov_island_prot, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island_prot, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_island_prot))

# 7. Full model: Island * Year * Protection
aov_full <- aov(Dens ~ Island * Year * Protection, data = ReefSlope_allyears)
cat("\n7. ANOVA: Full model (Island * Year * Protection)\n")
print(summary(aov_full))
eta_squared(aov_full, partial = FALSE)  # η² (general effect size)
eta_squared(aov_full, partial = TRUE)   # η²ₚ (partial effect size)

# ---------- Summary of P-Values ----------
cat("\n--- P-values ---\n")
cat("Year:                     ", round(p_year, 4), "\n")
cat("Island:                   ", round(p_island, 4), "\n")
cat("Protection:               ", round(p_protection, 4), "\n")
cat("Island * Year:            ", round(tidy(aov_island_year)$p.value[1:3], 4), "\n")
cat("Protection * Year:        ", round(tidy(aov_prot_year)$p.value[1:3], 4), "\n")
cat("Island * Protection:      ", round(tidy(aov_island_prot)$p.value[1:3], 4), "\n")
cat("Island * Year * Protect.: ", round(tidy(aov_full_no2018)$p.value[1:7], 4), "\n")

# ---------- Optional: Diagnostics ----------
# Uncomment to run
# par(mfrow = c(2,2))
# plot(aov_full)

#########################    NO 2018     #########################       #########################

ReefSlope_allyears_no2018 <-  ReefSlope_allyears %>%
  filter(Year != 2018)
  
#view(ReefSlope_allyears_no2018)
# 1. Year
aov_year_no2018 <- aov(Dens ~ Year, data = ReefSlope_allyears_no2018)
p_year_no2018 <- tidy(aov_year_no2018)$p.value[1]
cat("1. ANOVA: Year\n")
print(summary(aov_year_no2018))
eta_squared(aov_year_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_year_no2018, partial = TRUE)   # η²ₚ (partial effect size)
if (p_year_no2018 < 0.05) print(TukeyHSD(aov_year_no2018))

# 2. Island
aov_island_no2018 <- aov(Dens ~ Island, data = ReefSlope_allyears_no2018)
p_island_no2018 <- tidy(aov_island_no2018)$p.value[1]
cat("\n2. ANOVA: Island\n")
print(summary(aov_island_no2018))
eta_squared(aov_island_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island_no2018, partial = TRUE)   # η²ₚ (partial effect size)
if (p_island_no2018 < 0.05) print(TukeyHSD(aov_island_no2018))

# 3. Protection
aov_protection_no2018 <- aov(Dens ~ Protection, data = ReefSlope_allyears_no2018)
p_protection_no2018 <- tidy(aov_protection_no2018)$p.value[1]
cat("\n3. ANOVA: Protection\n")
print(summary(aov_protection_no2018))
eta_squared(aov_protection_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_protection_no2018, partial = TRUE)   # η²ₚ (partial effect size)
if (p_protection_no2018 < 0.05) print(TukeyHSD(aov_protection_no2018))

# 4. Island * Year
aov_island_year_no2018 <- aov(Dens ~ Island * Year, data = ReefSlope_allyears_no2018)
cat("\n4. ANOVA: Island * Year\n")
print(summary(aov_island_year_no2018))
eta_squared(aov_island_year_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island_year_no2018, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_island_year_no2018))

# 5. Protection * Year
aov_prot_year_no2018 <- aov(Dens ~ Protection * Year, data = ReefSlope_allyears_no2018)
cat("\n5. ANOVA: Protection * Year\n")
print(summary(aov_prot_year_no2018))
eta_squared(aov_prot_year_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_prot_year_no2018, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_prot_year_no2018))

# 6. Island * Protection
aov_island_prot_no2018 <- aov(Dens ~ Island * Protection, data = ReefSlope_allyears_no2018)
cat("\n6. ANOVA: Island * Protection\n")
print(summary(aov_island_prot_no2018))
eta_squared(aov_island_prot_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_island_prot_no2018, partial = TRUE)   # η²ₚ (partial effect size)
print(TukeyHSD(aov_island_prot_no2018))

# 7. Full model: Island * Year * Protection
aov_full_no2018 <- aov(Dens ~ Island * Year * Protection, data = ReefSlope_allyears_no2018)
cat("\n7. ANOVA: Full model (Island * Year * Protection)\n")
print(summary(aov_full_no2018))
eta_squared(aov_full_no2018, partial = FALSE)  # η² (general effect size)
eta_squared(aov_full_no2018, partial = TRUE)   # η²ₚ (partial effect size)

# ---------- Summary of P-Values ----------
cat("\n--- P-values ---\n")
cat("Year:                     ", round(p_year, 4), "\n")
cat("Island:                   ", round(p_island, 4), "\n")
cat("Protection:               ", round(p_protection, 4), "\n")
cat("Island * Year:            ", round(tidy(aov_island_year_no2018)$p.value[1:3], 4), "\n")
cat("Protection * Year:        ", round(tidy(aov_prot_year_no2018)$p.value[1:3], 4), "\n")
cat("Island * Protection:      ", round(tidy(aov_island_prot_no2018)$p.value[1:3], 4), "\n")
cat("Island * Year * Protect.: ", round(tidy(aov_full_no2018)$p.value[1:7], 4), "\n")
