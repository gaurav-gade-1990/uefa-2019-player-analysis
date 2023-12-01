library(lattice)
library(lme4)
install.packages("lmerTest") ##Need this package for p-value in lmer package
library(lmerTest)

#RDD Case Study 1: Create Dataset
fakedata <- expand.grid(day = seq(1,90,1), patient = seq(1,15,1))
fakedata$pt <- ifelse(fakedata$day >= 46,1,0)
fakedata$painlevel<- 70 - fakedata$day/3 + fakedata$pt*-15+ rnorm(1350,0,10)
fakedata$pt <- factor(fakedata$pt)

head(fakedata)
str(fakedata)

#Main Effect
xyplot(painlevel~day,fakedata,xlab="Day",ylab="Pain",pch=c(19,17),
       groups=pt,col=c("black","red"),type=c("p","r"),lwd=2,lty=c(2,1))
#Shift in the regression line at inflection point
#Trend lines don't diverge so no sign of interaction

#Test the model
#First difference out day by cutoff
fakedata$day2 <- fakedata$day - 45.5
#Run regression to see if main effect is indeed significant
model <- lmer(painlevel ~ day2*pt + (1|patient), data = fakedata)
summary(model)

#RDD Case Study 2: Create Dataset
fakedata2 <- expand.grid(day = seq(1,90,1), patient = seq(1,15,1))
fakedata2$pt <- ifelse(fakedata2$day >= 46,1,0)
fakedata2$painlevel <- 70 - fakedata2$day/(3-fakedata2$pt) + fakedata2$pt*-15+ rnorm(1350,0,10)

fakedata2$pt <- factor(fakedata2$pt)
head(fakedata2)
str(fakedata2)

#Check for Main Effects and Interactions
xyplot(painlevel~day,fakedata2,xlab="Day",ylab="Pain",pch=c(19,17), groups=pt,col=c("black","red"),type=c("p","r"),lwd=2,lty=c(2,1))

#Test the model
#Difference out day by cutoff
fakedata2$day2 <- fakedata$day - 45.5
#Run regression for main effect and interactions signifiance
model2 <- lmer(painlevel ~ day2*pt + (1|patient), data = fakedata2)
summary(model2)

##RDD Application #2
##Packages built under in MAR 2020
install.packages("rdd")
install.packages("rdrobust")
install.packages("rddtools")
library(rdd)
library(rdrobust)

modelr <- rdrobust(house$y, house$x, c = 0, all = TRUE)


data(house)
rdd_house <- rdd_data(x=x, y=y, data=house, cutpoint=0)
summary(rdd_house)
plot(rdd_house)
