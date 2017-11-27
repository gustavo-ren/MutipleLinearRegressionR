library(readr)
library(caTools)

#Data import
dataset <- read.csv("50_Startups.csv")
#Taking care of categorical string data
dataset$State = factor(dataset$State,
                       levels = c("New York", "California", "Florida"),
                       labels = c(1, 2, 3))
#Setting seed
set.seed(123)

#Spliting dataset based on the dependent variable
split <- sample.split(dataset$Profit, SplitRatio = 0.8)

trainingSet <- subset(dataset, split == TRUE)
testSet <- subset(dataset, split == FALSE)

#The . means "all the independent variables"
regression = lm(formula = Profit ~ .,data = trainingSet)

#The lower the p-value the more statistically significant the independent variable
#It can be check by the amount of stars in Signif. codes, more stars more significant
summary(regression)

y_pred = predict(regression, newdata = testSet)
View(y_pred)

#Creating a new regressions to compare the independent variables
regression2 = lm(formula = Profit ~ R.D.Spend, data = trainingSet)
summary(regression2)

y_pred2 = predict(regression2, newdata = testSet)
View(y_pred2)

regression3 = lm(formula = Profit ~ Administration + Marketing.Spend,
                 data = trainingSet)
summary(regression3)

y_pred3 = predict(regression3, newdata = testSet)
View(y_pred3)

regression4 = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                 data = trainingSet)
summary(regression4)

y_pred4 = predict(regression4, newdata = testSet)
View(y_pred4)

View(testSet)












