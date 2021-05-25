# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

house <- read.csv("LES_house_105_116.csv")
summary(house$les)
sd(house$les)

reg.house <- lm(les ~ leslag + dem + female + majority + votepct +
                  seniority + dwnom1 + chair + state_leg, data = house)
summary(reg.house)

reg2.house <- lm(all_bills ~ dem + female + majority + votepct +
                   seniority + dwnom1 + chair + state_leg, data = house)
summary(reg2.house)

# Senate

senate <- read.csv("LES_senate_105_116.csv")
summary(senate$les)
sd(senate$les)

summary(senate$all_bills)
sd(senate$all_bills)

reg.senate <- lm(les ~ leslag + dem + female + majority + votepct +
                  seniority + dwnom1 + chair + state_leg, data = senate)
summary(reg.senate)


reg2.senate <- lm(all_bills ~ dem + female + majority + votepct +
                   seniority + dwnom1 + chair + state_leg, data = senate)
summary(reg2.senate)
