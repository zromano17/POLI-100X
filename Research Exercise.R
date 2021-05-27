# Andrea Cendejas, Miguel Higuera, Zachary Romano
# POLI 100X
# Research Exercise
# May 28, 2021
################################################################################

# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load packages
library(lfe)

# Load datasets
congress <- read.csv("congress.csv")
legislation <- read.csv("legislation.csv")

# Merge datasets
data <- merge(congress, legislation, by = "cong")

# Create variable representing the difference in average DW-NOMINATE scores for the two parties
data$dw_diff <- data$r_mean - data$d_mean

# Create dummy variables for scontrol, hcontrol, and pcontrol
data$scontrol <- ifelse(data$scontrol == "D", 1, 0)
data$hcontrol <- ifelse(data$hcontrol == "D", 1, 0)
data$pcontrol <- ifelse(data$pcontrol == "D", 1, 0)

# Create a variable for decade
data$decade <- data$year - (data$year %% 10)

# Run linear regression with no controls
fit <- lm(n_landmark ~ dw_diff, data = data)
summary(fit)

###### FIRST PLOT ######
# Scatterplot of dw_diff and n_landmark
plot(data$dw_diff, data$n_landmark,
     xlab = "Difference in average DW-NOMINATE score between Republicans and Democrats",
     ylab = "Number of landmark legislation passed",
     pch = 19,
     col = "blue")
# Add linear model
abline(fit, lwd = 3)
# Add linear model equation
text(.95, 30, "n_landmark = 24.238 - 8.528(dw_diff)")

# Run linear regression with controls
fit.controls <- lm(n_landmark ~ dw_diff + scontrol + pcontrol + hcontrol, data = data)
summary(fit.controls)

###### SECOND PLOT ######
# Scatterplot of dw_diff and n_landmark
plot(data$dw_diff, data$n_landmark,
     xlab = "Difference in average DW-NOMINATE score between Republicans and Democrats",
     ylab = "Number of landmark legislation passed",
     pch = 19,
     col = "blue")
# Add new linear model (with controls)
abline(fit.controls, lwd = 3)
# Add new linear model equation
text(.8, 30, "n_landmark = 16.2143 - 1.3987(dw_diff) - 4.1250(scontrol) - 0.1218(pcontrol) + 8.7294(hcontrol)", cex = .9)

# Run linear regression with controls and decade-level fixed effects
fit.fe <- felm(n_landmark ~ dw_diff + scontrol + pcontrol + hcontrol | factor(decade), data = data)
summary(fit.fe)
