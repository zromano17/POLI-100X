# Zachary Romano
# POLI 100X
# Problem Set 2
# 4/30/2020
################################################################################

# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
data <- read.csv("sponsorship_house_101_116.csv")

############## (a) ###############

fit <- lm(all_bills ~ seniority + dem + majority + dwnom1, data = data)
summary(fit)

############## (b) ###############

fit2 <- lm(all_bills ~ female + seniority + dem + majority + dwnom1, data = data)
summary(fit2)

############## (e) ###############

fit3 <- lm(all_bills ~ female + seniority + dem + majority + dwnom1 + seniority:majority, data = data)
summary(fit3)
