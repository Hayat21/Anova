#read data

data <-read.csv("C:/Users/HP/Downloads/StudentsPerformance.csv")
mydata=na.omit(data)
gender=as.factor(mydata$gender)
race=as.factor(mydata$race)
math.score=(mydata$math.score)

#plot(table(data$race.ethnicity))

par(mfrow=c(1,2))
plot(race,math.score)
plot(gender,math.score)

par(mfrow=c(1,2))
interaction.plot(gender,race,math.score)
interaction.plot(race,gender,math.score)

table(race,gender)
res.aov2 <- aov(math.score~gender*race, data=mydata)
coef(res.aov2)
anova(res.aov2)
res.aov3 <- aov(math.score~gender*race*race:gender, data=mydata)
anova(res.aov3)


plot(res.aov2, 1)

install.packages("car")
library(car)
leveneTest(math.score ~ gender*race, data = mydata)

plot(res.aov3, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


