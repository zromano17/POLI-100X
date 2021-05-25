# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

social <- read.csv("social_neighbors.csv")

fit <- lm(primary2006 ~ primary2004 + neighbor, data = social)
summary(fit)

fit2 <- lm(primary2006 ~ primary2004 + neighbor + primary2004:neighbor, data = social)
summary(fit2)
