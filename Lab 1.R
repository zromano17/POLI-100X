# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
house <- read.csv("house_1946_2010.csv")

# Some preliminary practice

dim(house)
head(house)
summary(house)
summary(house$dvotes)
# mean(house$dvotes) will not work bc NAs
mean(house$dvotes, na.rm = TRUE)


# drop uncontested races (missing values for dvotes or rvotes)

# practice subsetting in an easier task
# subset to all races that had a Democratic incumbent
had_incumbent <- house[house$incumb == 1, ]

# subset to all races that had a Democratic incumbent AND was redistricted
had_incumbent_redist <- house[house$incumb ==1 & house$redist == 1, ]

# let's start with just getting rows that DON'T have missing values for dvotes
# (exclude races that had missing values for dvotes)

houseraces <- house[is.na(house$dvotes) == FALSE, ]
houseraces <- house[is.na(house$dvotes) == FALSE & is.na(house$rvotes) == FALSE, ]
houseraces <- house[is.na(house$dvotes) == FALSE & is.na(house$rvotes) == FALSE & is.na(house$d_vshare_prev) == FALSE, ]

houseraces <- house[is.na(house$dvotes) == FALSE & is.na(house$rvotes) == FALSE & 
                      is.na(house$d_vshare_prev) == FALSE & house$redist == 0), ]

# Generate a new variable

houseraces$d_vshare <- houseraces$dvotes / (houseraces$dvotes + houseraces$rvotes)
summary(houseraces$d_vshare)
sd(houseraces$d_vshare)

hist(houseraces$d_vshare, freq = TRUE)
hist(houseraces$d_vshare, freq = TRUE, main = "Democratic Vote Share, 1946-2010", xlab = "Democratic Vote Share")
abline(v=mean(houseraces$d_vshare), lwd = 3)
text(x = 0.6, y = 1000, "mean")

