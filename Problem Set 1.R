# Zachary Romano
# POLI 100X
# Problem Set 1
# 4/16/2020
################################################################################

# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
house <- read.csv("house_1946_2010.csv")

############## (a) ###############

# Drop all uncontested elections and elections for which the previous election was uncontested
house <- house[is.na(house$dvotes) == FALSE & is.na(house$rvotes) == FALSE & is.na(house$d_vshare_prev) == FALSE,]

# Drop all elections if the district was redrawn since the last election
house <- house[house$redist != 1, ]

# Check the number of remaining observations
dim(house)

############## (b) ###############

# Generate a variable for the Democratic two-party vote share
house$d_vshare <- house$dvotes / (house$dvotes + house$rvotes)

# Check the number of observations
length(house$d_vshare)

# Report summary statistics (mean, max, min, sd) for d_vshare
summary(house$d_vshare)
sd(house$d_vshare)

############## (d) ###############

# Regress the Democratic party vote share on the Democratic party incumbency status, the previous Democratic party vote share, and the district normal vote
fit <- lm(d_vshare ~ incumb + d_vshare_prev + dpres, data = house)
summary(fit)

############## (f) ###############
# Create a new dataset for elections from 1946 to 1978
house_1946_1978 <- house[house$year <= 1978, ]

# Repeat part (d) for the elections from 1946 to 1978
fit1 <- lm(d_vshare ~ incumb + d_vshare_prev + dpres, data = house_1946_1978)
summary(fit1)

# Create a new dataset for elections from 1980 to 2010
house_1980_2010 <- house[house$year >= 1980 & house$year <= 2010, ]

# Repeat part (d) for the elections from 1980 to 2010
fit2 <- lm(d_vshare ~ incumb + d_vshare_prev + dpres, data = house_1980_2010)
summary(fit2)
