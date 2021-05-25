# install.packages("lfe")
library(lfe)

setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 
df <- read.csv("Ban_Grimmer_Kaslovsky_West.csv")
df2 <- read.csv("avg_times_interrupting.csv")

fit.controls<- lm(avg_interruptions ~ percwomen + nokken_poole_dim1 + majority + 
                    voteshare + chair, data = df)
summary(fit.controls)

fit.fe <- felm(avg_interruptions ~ percwomen + nokken_poole_dim1 + majority +
                 voteshare + chair | factor(memberid), data = df)
summary(fit.fe)

fit.fe2 <- felm(avg_interruptions ~ percwomen + nokken_poole_dim1 + majority +
                 voteshare + chair | factor(memberid) + factor(committee_name), data = df)
summary(fit.fe2)

fit.fe3 <- felm(avg_interruptions ~ percwomen + nokken_poole_dim1 + majority +
                  voteshare + chair | factor(memberid) + factor(committee_name) + factor(cong), data = df)
summary(fit.fe3)

df.merged <- merge(df, df2, by = c("cong", "memberid", "committee_name"), all.x = TRUE)

fit.fe4 <- felm(avg_times_interrupting ~ percwomen + nokken_poole_dim1 + majority +
                  voteshare + chair | factor(memberid) + factor(committee_name) + factor(cong), data = df.merged)
summary(fit.fe4)
