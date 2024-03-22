#data preprosessing
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students
#x=math records
#Y=port records
range(d3$Medu)
print(paste("No outliers in Medu(Mother's education). Values in the range",min(d3$Medu),"and",max(d3$Medu)))
range(d3$Fedu)
print(paste("No outliers in Fedu(Father's education). Values in the range",min(d3$Fedu),"and",max(d3$Fedu)))
range(d3$traveltime.x)
print(paste("No outliers in traveltime(home to school travel time) in x dataset. Values in the range",min(d3$traveltime.x),"and",max(d3$traveltime.x)))
range(d3$traveltime.y)
print(paste("No outliers in traveltime(home to school travel time) in y dataset. Values in the range",min(d3$traveltime.y),"and",max(d3$traveltime.y)))
range(d3$studytime.x)
print(paste("No outliers in studytime(weekly study time) in x dataset. Values in the range",min(d3$studytime.x),"and",max(d3$studytime.x)))
range(d3$studytime.y)
print(paste("No outliers in studytime(weekly study time) in y dataset. Values in the range",min(d3$studytime.y),"and",max(d3$studytime.y)))
range(d3$failures.x)
print(paste("No outliers in failures(number of past class failures) in x dataset. Values in the range",min(d3$failures.x),"and",max(d3$failures.x)))
range(d3$famrel.x)
print(paste("No outliers in famrel(quality of family relationships) in x dataset. Values in the range",min(d3$famrel.x),"and",max(d3$famrel.x)))
range(d3$freetime.x)
print(paste("No outliers in freetime(free time after school) in x dataset. Values in the range",min(d3$freetime.x),"and",max(d3$freetime.x)))
range(d3$goout.x)
print(paste("No outliers in goout(going out with friends) in x dataset. Values in the range",min(d3$goout.x),"and",max(d3$goout.x)))
range(d3$G1.x)
print(paste("No outliers in G1(first period grade) in x dataset. Values in the range",min(d3$G1.x),"and",max(d3$G1.x)))
range(d3$G1.y)
print(paste("No outliers in G1(first period grade) in y dataset. Values in the range",min(d3$G1.y),"and",max(d3$G1.y)))
range(d3$G2.x)
print(paste("No outliers in G2(second period grade) in y dataset. Values in the range",min(d3$G2.x),"and",max(d3$G2.x)))
c1 <- range(d3$Dalc.x)
print(paste("no outliers in workday alcohol consumption in the X dataset.values are within range",c1[1]," to ",c1[2]))
c2<- range(d3$Dalc.y)
print(paste("no outliers in  workday alcohol consumption in the Y dataset.values are within range",c2[1]," to ",c2[2]))
c3<- range(d3$Walc.x)
print(paste("no outliers in  weekend alcohol consumption in the X dataset.values are within range",c3[1]," to ",c3[2]))
c4<- range(d3$Walc.y)
print(paste("no outliers in weekend alcohol consumption in the Y dataset.values are within range",c4[1]," to ",c4[2]))
c5 <-range(d3$health.x)
print(paste("no outliers in health status in the X dataset.values are within range",c5[1]," to ",c5[2]))
c6<-range(d3$health.y)
print(paste("no outliers in health status in the Y dataset.values are within range",c6[1]," to ",c6[2]))
c7 <-range(d3$absences.x)
print(paste("no outliers in absence records in the X dataset.values are within range",c7[1]," to ",c7[2]))
c8 <-range(d3$absences.y)
print(paste("no outliers in absence records in the Y dataset.values are within range",c8[1]," to ",c8[2]))
c9<-range(d3$G3.x)
print(paste("no outliers in Final grades in the X dataset.values are within range",c9[1]," to ",c9[2]))
c10<-range(d3$G3.y)
print(paste("no outliers in Final grades in the Y dataset.values are within range",c10[1]," to ",c10[2]))
c11<-range(d3$failures.y)
print(paste("no outliers in Failures in the Y dataset.values are within range",c11[1]," to ",c11[2]))
c12<-range(d3$famrel.y)
print(paste("no outliers in quality of family relation in the Y dataset.values are within range",c12[1]," to ",c12[2]))
c13<-range(d3$freetime.y)
print(paste("no outliers in quality of freetime relation in the Y dataset.values are within range",c13[1]," to ",c13[2]))
c14<-range(d3$goout.y)
print(paste("no outliers in go out relation in the Y dataset.values are within range",c14[1]," to ",c14[2]))
c15<-range(d3$G2.y)
print(paste("no outliers in second period grade in the Y dataset.values are within range",c15[1]," to ",c15[2]))
#calculating average scores of students in math subject
for(i in 1:382){
d3$G4.y[i] <- (d3$G1.y[i]+d3$G2.y[i]+d3$G3.y[i])/3.0
d3$G4.x[i] <- (d3$G1.x[i]+d3$G2.x[i]+d3$G3.x[i])/3.0
}
#eda
summary(d3)
data(d3)

mean(d3$G4.y)
mean(d3$G4.x)
median(d3$G4.x)
median(d3$G4.y)
mode(d3$G4.y)
mode(d3$G4.x)
str(d3)
var(d3$G4.x, na.rm=TRUE)
sd(d3$G4.x, na.rm = TRUE)
cv <- sd(d3$G4.x) / mean(d3$G4.x) * 100
cv
quantile(d3$G4.x)
range(d3$G4.x)
IQR(d3$G4.x)
sort(unique(d3$Walc.x))
var(d3$G4.y, na.rm=TRUE)
sd(d3$G4.y, na.rm = TRUE)
cv <- sd(d3$G4.y) / mean(d3$G4.y) * 100
cv
quantile(d3$G4.y)
range(d3$G4.y)
IQR(d3$G4.y)
sort(unique(d3$studytime.y))
install.packages("ggplot2")
library(ggplot2)
# graphs
# bar plot
ggplot(d3, aes(x = G4.x)) +geom_bar()
ggplot(d3, aes(x = G4.y)) +geom_bar()
ggplot(d3, aes(x = G1.x)) +geom_bar()
ggplot(d3, aes(x = G1.y)) +geom_bar()
ggplot(d3, aes(x = G2.x)) +geom_bar()
ggplot(d3, aes(x = G2.y)) +geom_bar()
ggplot(d3, aes(x = G3.x)) +geom_bar()
ggplot(d3, aes(x = G3.y)) +geom_bar()
# histogram
ggplot(d3, aes(x = G4.x)) +geom_histogram(bins = nclass.Sturges(d3$G4.x))
ggplot(d3, aes(x = G4.y)) +geom_histogram(bins = nclass.Sturges(d3$G4.y))
# density curve
ggplot(d3, aes(x = G4.x)) + geom_density()
ggplot(d3, aes(x = G4.y)) + geom_density()
ggplot(d3, aes(x = G3.x)) + geom_density()
ggplot(d3, aes(x = G3.y)) + geom_density()
ggplot(d3, aes(x = G2.x)) + geom_density()
ggplot(d3, aes(x = G2.y)) + geom_density()
ggplot(d3, aes(x = G1.x)) + geom_density()
ggplot(d3, aes(x = G1.y)) + geom_density()
# box plot
ggplot(d3, (aes(x = Walc.x, y =G4.x))) +geom_boxplot()
ggplot(d3, (aes(x = Walc.y, y =G4.y))) +geom_boxplot()
ggplot(d3, (aes(x = freetime.x, y =G4.x))) +geom_boxplot()
ggplot(d3, (aes(x = freetime.y, y =G4.y))) +geom_boxplot()
ggplot(d3, (aes(x = studytime.x, y =G4.x))) +geom_boxplot()
ggplot(d3, (aes(x = studytime.y, y =G4.y))) +geom_boxplot()
# QQ Plot for Clay
ggplot(d3, aes(sample = G4.x)) +geom_qq() +geom_qq_line()
ggplot(d3, aes(sample = G4.y)) +geom_qq() +geom_qq_line()
#feature selection using r part algorithm
install.packages("Boruta")
install.packages("mlbench")
install.packages("caret")
install.packages("randomForest")
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
# Perform r part model search
install.packages("caTools")
set.seed(2)
library(caTools)
spli<- sample.split(d3,SplitRatio=0.7)
tr <-sample.split(spli,)
botrain <- subset(d3,spli="TRUE")
tdata <-train(sex ~ .,data=botrain,method="rpart")
rpartimp <- varImp(tdata)
#model development
split= sample.split(d3, SplitRatio = 0.7)
trainingset = subset(d3, split == "TRUE")
testset = subset(d3, split == "FALSE")

# Fitting Simple Linear Regression to the Training set for walc
lm.r1= lm(formula = G4.x~Walc.x,data = trainingset)
coef(lm.r1)
summary(lm.r1)
# Predicting the Test set results
ypred = predict(lm.r1, newdata = testset)
plot(ypred,col="red",type="l")
summary(ypred)

lm.r2= lm(formula = G4.y~Walc.y,data = trainingset)
coef(lm.r2)
summary(lm.r2)
# Predicting the Test set results
ypred = predict(lm.r2, newdata = testset)
plot(ypred,col="black",type="l")
summary(ypred)
#linear regression on study time 
lm.r3= lm(formula = G4.x~studytime.x,data = trainingset)
coef(lm.r3)
summary(lm.r3)
# Predicting the Test set results
ypred = predict(lm.r3, newdata = testset)
plot(ypred,col="blue",type="l")
summary(ypred)
lm.r4= lm(formula = G4.y~studytime.y,data = trainingset)
coef(lm.r4)
summary(lm.r4)
# Predicting the Test set results
ypred = predict(lm.r4, newdata = testset)
plot(ypred,col="darkgreen",type="l")
summary(ypred)
#free time
lm.r5= lm(formula = G4.x~freetime.x,data = trainingset)
coef(lm.r5)
summary(lm.r5)
# Predicting the Test set results
ypred = predict(lm.r5,testset)
plot(ypred,col="darkred",type="l")
summary(ypred)
lm.r6= lm(formula = G4.y~freetime.y,data = trainingset)
coef(lm.r6)
summary(lm.r6)
# Predicting the Test set results
ypred = predict(lm.r6, newdata = testset)
plot(ypred,col="darkblue",type="l")
summary(ypred)

#SVM of Walc.x
install.packages("tidyverse")
install.packages("e1071")
#create a is sample indicator
sam <- c(rep(-1,10),rep(+1,10))
length(sam) <- length(d3$Walc.x)
library(e1071)
my.data <- data.frame(G4.x= d3['G4.x'],
                      Walc.x= d3['Walc.x'],
                      Type=as.factor(sam))
my.data
#plot the data
plot(my.data[,-3],col=(3)/2,pch=19); abline(h=0,v=0,lty=3)
#perform svm by calling the svm method and passing the parameters
svm.model <- svm(Type~., 
                 data=my.data, 
                 type='C-classification',
                 kernel='linear',
                 scale=FALSE)
#display summary of svm
summary(svm.model)
#show the support vectors
points(my.data[svm.model$index, c(1,2)] , col="orange", cex=2)
#get parameters of the hyperplane
w <- t(svm.model$coefs)%*% svm.model$SV
b <- svm.model$rho
#in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*2+b=0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col='blue', lty=3)
#predict the results
predict(svm.model)

#SVM of Walc.y
sam <- c(rep(-1,10),rep(+1,10))
length(sam) <- length(d3$Walc.y)
library(e1071)
my.data <- data.frame(G4.y= d3['G4.y'],
                      Walc.y= d3['Walc.y'],
                      Type=as.factor(sam))
my.data
#plot the data
plot(my.data[,-3],col=(3)/2,pch=19); abline(h=0,v=0,lty=3)
#perform svm by calling the svm method and passing the parameters
svm.model1 <- svm(Type~., 
                  data=my.data, 
                  type='C-classification',
                  kernel='linear',
                  scale=FALSE)
#display summary of svm
summary(svm.model1)
#show the support vectors
points(my.data[svm.model1$index, c(1,2)] , col="orange", cex=2)
#get parameters of the hyperplane
w <- t(svm.model1$coefs)%*% svm.model1$SV
b <- svm.model1$rho
#in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*2+b=0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col='blue', lty=3)
#predict the results
predict(svm.model1)

#SVM of freetime.x
sam <- c(rep(-1,10),rep(+1,10))
length(sam) <- length(d3$freetime.x)
library(e1071)
my.data <- data.frame(G4.x= d3['G4.x'],
                      freetime.x= d3['freetime.x'],
                      Type=as.factor(sam))
my.data
#plot the data
plot(my.data[,-3],col=(3)/2,pch=19); abline(h=0,v=0,lty=3)
#perform svm by calling the svm method and passing the parameters
svm.model2 <- svm(Type~., 
                  data=my.data, 
                  type='C-classification',
                  kernel='linear',
                  scale=FALSE)
#display summary of svm
summary(svm.model2)
#show the support vectors
points(my.data[svm.model2$index, c(1,2)] , col="orange", cex=2)
#get parameters of the hyperplane
w <- t(svm.model2$coefs)%*% svm.model2$SV
b <- svm.model2$rho
#in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*2+b=0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col='blue', lty=3)
#predict the results
predict(svm.model2)

#SVM of freertime.y
sam <- c(rep(-1,10),rep(+1,10))
length(sam) <- length(d3$freetime.y)
library(e1071)
my.data <- data.frame(G4.y= d3['G4.y'],
                      freetime.y= d3['freetime.y'],
                      Type=as.factor(sam))
my.data
#plot the data
plot(my.data[,-3],col=(3)/2,pch=19); abline(h=0,v=0,lty=3)
#perform svm by calling the svm method and passing the parameters
svm.model3 <- svm(Type~., 
                  data=my.data, 
                  type='C-classification',
                  kernel='linear',
                  scale=FALSE)
#display summary of svm
summary(svm.model3)
#show the support vectors
points(my.data[svm.model3$index, c(1,2)] , col="orange", cex=2)
#get parameters of the hyperplane
w <- t(svm.model3$coefs)%*% svm.model3$SV
b <- svm.model3$rho
#in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*2+b=0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col='blue', lty=3)
#predict the results
predict(svm.model3)

#Polynomial regression of Walc.x
plot(d3$G4.x~d3$Walc.x)
lines(lowess(d3$G4.x~d3$Walc.x))
y <- lm(G4.x ~ Walc.x + I(G4.x^2), data=d3)
summary(y)
lines(d3$Walc.x, predict(y))
lines(d3$Walc.x, predict(y),col=2)

#Polynomial regression of Walc.y
plot(d3$G4.y~d3$Walc.y)
lines(lowess(d3$G4.y~d3$Walc.y))
z <- lm(G4.y ~ Walc.y + I(G4.y^2), data=d3)
summary(z)
lines(d3$Walc.y, predict(z))
lines(d3$Walc.y, predict(z),col=2)

#Polynomial regression of freetime.x
plot(d3$G4.x~d3$freetime.x)
lines(lowess(d3$G4.x~d3$freetime.x))
h <- lm(G4.x ~ freetime.x + I(G4.x^2), data=d3)
summary(h)
lines(d3$freetime.x, predict(h))
lines(d3$freetime.x, predict(h),col=2)

#Polynomial regression of freetime.y
plot(d3$G4.y~d3$freetime.y)
lines(lowess(d3$G4.y~d3$freetime.y))
k <- lm(G4.y ~ freetime.y + I(G4.y^2), data=d3)
summary(k)
lines(d3$freetime.y, predict(k))
lines(d3$freetime.y, predict(k),col=2)


#MODEL COMPARISON
#linear walc.x vs svm walc.x
anova(lm.r1,svm.model,test='F')
#linear walc.y vs svm walc.y
anova(lm.r2,svm.model1,test='F')
#linear freetime.x vs svm freetime.x
anova(lm.r5,svm.model2,test='F')
#linear freetime.y vs svm freetime.y
anova(lm.r6,svm.model3,test='F')
#linear freetime.x vs svm freetime.x
anova(lm.r5,svm.model2,test='F')