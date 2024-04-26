rm(list=ls()) # removes all variables stored previously
library(Hmisc)

data <- read.csv("C:/Users/vince/OneDrive/Desktop/Data/Raw Data/COVID19.csv")

describe(data) #Hmisc command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)

# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level =  0.95)
# 95% chance individual alive is 16 to 24 years younger than a dead person
# p-value 2.2e-16 shows extremely low probability our result is extreme
# reject null hypothesis and conclude this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level =  0.95)
# 95% confidence: men have 1.7% to 7.8% higher chance of dying than women
# p-value = 0.002 < 0.05, so we reject the null
# this is statistically significant
