install.packages("psych")
library(psych)

setwd("C:\\Users\\devan\\Desktop")

data <- read.table("SalaryThreePredictors.csv", sep=",",header=T)

hist(data$Salary, breaks = 15)
hist(data$Time, breaks = 15)
hist(data$Citations, breaks = 15)
hist(data$Pubs, breaks = 15)

qqnorm(data$Salary)
qqline(data$Salary)

qqnorm(data$Time)
qqline(data$Time)

qqnorm(data$Pubs)
qqline(data$Pubs)

qqnorm(data$Citations)
qqline(data$Citations)

#Get the correlation matrix
corr.test(data)

reg1 <- lm(data$Salary ~ data$Time + data$Pubs)
summary(reg1)

reg2 <- lm(data$Salary ~ data$Time + data$Pubs + data$Citations)
summary(reg2)

anova(reg1, reg2)
