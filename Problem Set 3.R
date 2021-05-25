# Zachary Romano
# POLI 100X
# Problem Set 3
# 5/14/2020
################################################################################

# Set working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
house <- read.csv("house_1946_2010.csv")

# Load in necessary libraries
library(lfe)

############## (a) ###############

# Drop all uncontested elections and elections for which the previous election was uncontested
house <- house[is.na(house$dvotes) == FALSE & is.na(house$rvotes) == FALSE & is.na(house$d_vshare_prev) == FALSE,]

# Drop all elections if the district was redrawn since the last election
house <- house[house$redist != 1, ]

# Generate a variable for the Democratic two-party vote share
house$d_vshare <- house$dvotes / (house$dvotes + house$rvotes)

############## (c) ###############

fit <- felm(d_vshare ~ incumb + d_vshare_prev | factor(stcd), data = house)
summary(fit)

############## (e) ###############

fit2 <- felm(d_vshare ~ incumb + d_vshare_prev | factor(stcd) + factor(year), data = house)
summary(fit2)
