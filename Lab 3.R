rm(list = ls())

# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
df <- read.csv("npat_105.csv")

plot(df$score_npat, df$score_close_rolls, ylab = "Close roll-call vote score",
     xlab = "NPAT score", col = ifelse(df$party == 100, "blue", "red"), 
     main = "NPAT Scores and Voting \nBehavior on Close Roll Call Votes")

fit <- lm(score_close_rolls ~ score_npat, data = df)
summary(fit)

df$dem <- ifelse(df$party == 100, 1, 0)

fit2 <- lm(score_close_rolls ~ dem + score_npat, data = df)
summary(fit2)
