library(foreign)
library(rdd)

setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

df <- read.dta("Hall_primary_analysis.dta")

# scatter plot

plot(df$winner_hall_snyder_score, df$dwnom1, xlab = "Contribution-based ideology score", ylab = "1st dimension DW-NOMINATE", 
     col = ifelse(df$dem == 1, "blue", "red"))

# add line of best fit

fit.ols <- lm(dwnom1 ~ winner_hall_snyder_score, data = df)
summary(fit.ols)
plot(df$winner_hall_snyder_score, df$dwnom1, xlab = "Contribution-based ideology score", ylab = "1st dimension DW-NOMINATE", 
     col = ifelse(df$dem == 1, "blue", "red"))
abline(fit.ols, col = "black")

# subset data

rd.data <- df[df$margin <= .05 & df$absdist >= .08, ]

# create indicator variable for whether or not treatment occured (treatment = extremist in general election)
rd.data$treat <- ifelse(rd.data$rv > 0, 1, 0)
rd.data$rv_treat <- rd.data$treat * rd.data$rv

fit.rd <- RDestimate(dv ~ rv | treat + rv_treat, data = rd.data, cutpoint = 0)
summary(fit.rd)
