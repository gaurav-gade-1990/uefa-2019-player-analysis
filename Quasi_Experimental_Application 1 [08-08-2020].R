library(Hmisc)

#2LSL Example
head(QUASI1)
str(QUASI1)

#Check for Correlations
#cor(QUASI1)
DataMatrix <- as.matrix(data.frame(QUASI1$employee,QUASI1$salary,QUASI1$enjoymentstd,QUASI1$timeonjob,QUASI1$education))
rcorr(DataMatrix)

#Improper Example
model <- lm(enjoymentstd ~ salary + timeonjob + education, data = QUASI1)
summary(model)

#First Regression (Predict Pay by Time on Job and Education)
model1 <- lm(salary ~ timeonjob + education, data = QUASI1)
summary(model1)
#Generate Predicted Salary
QUASI1$predicted <- predict(model1, QUASI1)
model2 <- lm(enjoymentstd ~ predicted + timeonjob, data = QUASI1)
summary(model2)





