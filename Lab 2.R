# Set the working directory
setwd("C:/Users/zroma/OneDrive/Desktop/School/POLI 100X") 

# Load the csv data file
dwnom <- read.csv("dwnom_87_113.csv")

# subset to 113th Congress
dwnom113 <- dwnom[dwnom$cong==113, ]

# histogram of the first dimension DW-NOMINATE scores for the 113th Congress
hist(dwnom113$dwnom1, freq = TRUE, main = "DW-NOMINATE 1st dimension, 113th Congress", xlab = "DW-NOMINATE 1st dimension")

# generate a dummy variable repub that equals 1 if the member is from the Republican party, and equal 0 otherwise
dwnom$repub <- ifelse(dwnom$party == 200, 1, 0)

# regress the 1st dimension DW-NOMINATE score on the Republican party dummy
fit <- lm(dwnom1 ~ repub, data = dwnom)
fit
summary(fit)

# regress the 1st dimension DW-NOMINATE score on the Republican party dummy and south dummy
fit2 <- lm(dwnom1 ~ repub + south, data = dwnom)
summary(fit2)

# subset to 87th Congress
dwnom87 <- dwnom[dwnom$cong == 87, ]
fit_87 <- lm(dwnom1 ~ repub + south, data = dwnom87)
summary(fit_87)

# generate a scatterplot for the 113th congress (both dimensions of DW-NOMINATE)
plot(dwnom113$dwnom1, dwnom113$dwnom2, pch = 16, main = "DW NOMINATE scores of the 113th Congress",
     xlab = "1st dimension DW-NOMINATE", ylab = "2nd dimension DW-NOMINATE")

plot(dwnom113$dwnom1, dwnom113$dwnom2, pch = 16, main = "DW NOMINATE scores of the 113th Congress",
     xlab = "1st dimension DW-NOMINATE", ylab = "2nd dimension DW-NOMINATE", col = ifelse(dwnom113$party == 200, "red", "black"))
  
plot(dwnom113$dwnom1, dwnom113$dwnom2, pch = 16, main = "DW NOMINATE scores of the 113th Congress",
     xlab = "1st dimension DW-NOMINATE", ylab = "2nd dimension DW-NOMINATE", col = ifelse(dwnom113$party == 200, "red", ifelse(dwnom113$party == 100, "blue", "black")))

     