# Load in libraries
library(foreign)
library(rdd)

# Set working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

df <- read.dta("Senate_candidates.dta")

# linear regression
fit.ols <- lm(became_lobbyist ~ winner, data = df)
summary(fit.ols)

# generate interaction
df$vote_margin_winner = df$vote_margin * df$winner

fit.rdd <- RDestimate(became_lobbyist ~ vote_margin | winner + vote_margin_winner, data = df, cutpoint = 0)
summary(fit.rdd)
