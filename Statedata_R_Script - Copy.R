
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#---------------------Setting Working Directory--------------------------------------------------
setwd("E:/Ivyproschool/R/Linear Regression/Case Study-1")
getwd()

data=read.csv("statedata.csv")
data1=data

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)

#---------------------------Outlier treatment-------------------------------------------------------
library(graphics)
attach(data1)
boxplot(data1)
boxplot(Population)##outlier
boxplot(Income)###outlier
boxplot(Illiteracy)#no
boxplot(Murder)#no
boxplot(HS.Grad)#no
boxplot(Frost)#no
boxplot(Area)###outlier present
boxplot(Life.Exp)#no

summary(data1$Area)
bench1<- 81163+1.5*IQR(data1$Area)
data1$Area>147428.9 
#3 values are present so treated the outlier
data1$Area<-ifelse(data1$Area>147428.9,147428.9,data1$Area)

summary(data1$Income)
bench2<- 4814+1.5*IQR(data1$Income)
data1$Income>6045.125 
#1 value is present so treated
data1$Income=ifelse(data1$Income>6045.125,6045.125,data1$Income)

summary(data1$Population)
bench3<- 4968+1.5*IQR(data1$Population)
data1$Population> 10801.5 
#5 values are present so treated  the outlier with 3rd quartile value
data1$Population=ifelse(data1$Population>10801.5,10801.5,data1$Population)


#-------------->Droping the rendundant variables from the data frame

data1=subset(data1,select = -c(x,y,state.abb,state.area,state.name,state.region))

#----------------Converting variables into factors-----------------#

data1[,"state.division"]<-as.factor(data1[,"state.division"])

#-------------->Missing values Identification and Treatment

sapply(data1,function(x)sum(is.na(x)))
str(data1)#final dataset for modelling
as.data.frame(colnames(data1))

#--------------------------Splitting the data into training and test data set------------------------#
set.seed(123)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data1$Life.Exp, 0.8)#Splits the overall data into train and test data in 80:20 ratio

original.data = subset(data1, spl == TRUE)
str(original.data)
dim(original.data)

test.data = subset(data1, spl == FALSE)
str(test.data)
dim(test.data)


#------------------------------------------Fitting the model on train data---------------------------------------#
#Iteration.1 We start with testing all variables
options(scipen = 999)

LinearModel0=lm(Life.Exp~. ,data=original.data)
summary(LinearModel0)

#Iteration 2 Removing insignificant variable       I(state.division=="Pacific")+
LinearModel1=lm(Life.Exp~Population+Illiteracy+Income+Murder+HS.Grad+Frost+Area+
              I(state.division=="East South Central")+
              I(state.division=="Middle Atlantic ")+
              I(state.division=="New England")+
              I(state.division=="Mountain")+
              I(state.division=="South Atlantic")+
              I(state.division=="West North Central")+
              I(state.division=="West South Central"),data=original.data)
summary(LinearModel1)

#Iteration 3 Removing insignificant variable state.division=="Middle Atlantic" 
LinearModel2=lm(Life.Exp~Population+Illiteracy+Murder+HS.Grad+Frost+Area+Income+
                  I(state.division=="East South Central")+
                  I(state.division=="New England")+
                  I(state.division=="Mountain")+
                  I(state.division=="South Atlantic")+
                  I(state.division=="West North Central")+
                  I(state.division=="West South Central"),data=original.data)
summary(LinearModel2)

#Iteration 4 Removing insignificant variable Area 
LinearModel3=lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+
                  I(state.division=="East South Central")+
                  I(state.division=="New England")+
                  I(state.division=="Mountain")+
                  I(state.division=="South Atlantic")+
                  I(state.division=="West North Central")+
                  I(state.division=="West South Central"),data=original.data)
summary(LinearModel3)

#Iteration 5 Removing insignificant variable Illiteracy
LinearModel4=lm(Life.Exp~Population+Murder+HS.Grad+Frost+
                  I(state.division=="East South Central")+
                  I(state.division=="New England")+
                  I(state.division=="Mountain")+
                  I(state.division=="South Atlantic")+
                  I(state.division=="West North Central")+
                  I(state.division=="West South Central"),data=original.data)
summary(LinearModel4)

#Iteration 6 Removing insignificant variable  I(state.division=="South Atlantic")
LinearModel5=lm(Life.Exp~Population+Murder+HS.Grad+Frost+
                  I(state.division=="East South Central")+
                  I(state.division=="New England")+
                  I(state.division=="Mountain")+
                  I(state.division=="West North Central")+
                  I(state.division=="West South Central"),data=original.data)
summary(LinearModel5)

#Iteration 7 Removing insignificant variable  I(state.division=="New England")+
LinearModel6=lm(Life.Exp~Population+Murder+HS.Grad+Frost+
                  I(state.division=="East South Central")+
                  I(state.division=="Mountain")+
                  I(state.division=="West South Central")+
                  I(state.division=="West North Central"),data=original.data)
summary(LinearModel6)

#Iteration 8 Removing insignificant variable   I(state.division=="Mountain")+
LinearModel7=lm(Life.Exp~Population+Murder+HS.Grad+Frost+
                  I(state.division=="East South Central")+
                  I(state.division=="West South Central")+
                  I(state.division=="West North Central"),data=original.data)
summary(LinearModel7)

#Iteration 9 Removing insignificant variable Frost
LinearModel8=lm(Life.Exp~Population+Murder+HS.Grad+
                  I(state.division=="East South Central")+
                  I(state.division=="West South Central")+
                  I(state.division=="West North Central"),data=original.data)
summary(LinearModel8)

#---------Final Model--------
#Iteration 9 Removing insignificant variable Frost
Finalmodel=lm(Life.Exp~Population+Murder+HS.Grad+
                  I(state.division=="East South Central")+
                  I(state.division=="West South Central")+
                  I(state.division=="West North Central"),data=original.data)
summary(Finalmodel)

#Checking for multicollinearilty

vif(Finalmodel)

#Checking for autocorrelation
durbinWatsonTest(Finalmodel)

#Calculating error
original.data$pred <- fitted(Finalmodel) #predicted value of life.exp
Actual_Pred<-dplyr::select(original.data,c(Life.Exp,pred))
Actual_Pred$error<-Actual_Pred$Life.Exp-Actual_Pred$pred
summary(Actual_Pred$error)
write.csv(data1,"mape.csv")

#calculating MAPe
attach(original.data)
MAPE<-print((sum((abs(Life.Exp-pred))/Life.Exp))/nrow(original.data))

#MAPE=0.006361254

## Get the predicted or fitted values
fitted(Finalmodel)

par(mfrow=c(2,2))
plot(Finalmodel)

###########---Checking the model on TEST data------##############

fit1=lm(Life.Exp~Population+Murder+HS.Grad+
                I(state.division=="East South Central")+
                I(state.division=="West South Central")+
                I(state.division=="West North Central"),data=test.data)
summary(fit1)

#Iteration 1 Removing insignificant variable population


fit2=lm(Life.Exp~Murder+HS.Grad+
          I(state.division=="East South Central")+
          I(state.division=="West South Central")+
          I(state.division=="West North Central"),data=test.data)
summary(fit2)
#Iteration 2 Removing insignificant variable  I(state.division=="West North Central")

fit3=lm(Life.Exp~Murder+HS.Grad+
          I(state.division=="East South Central")+
          I(state.division=="West South Central"),data=test.data)
summary(fit3)

#Iteration 2 Removing insignificant variable  I(state.division=="East South Central")

fit4=lm(Life.Exp~Murder+HS.Grad+
          I(state.division=="West South Central"),data=test.data)
summary(fit4)

#Iteration 2 Removing insignificant variable  Murder

fit5=lm(Life.Exp~HS.Grad+I(state.division=="West South Central"),data=test.data)
summary(fit5)

#Checking for multicollinearity
vif(fit5)
#Checking for autocorrelation
durbinWatsonTest(fit5)

#Calculating error
test.data$pred <- fitted(fit5) #predicted value of life.exp
Actual_Pred<-dplyr::select(test.data,c(Life.Exp,pred))
Actual_Pred$error<-Actual_Pred$Life.Exp-Actual_Pred$pred
summary(Actual_Pred$error)
write.csv(data1,"mape.csv")

#calculating MAPe
attach(test.data)
MAPE<-print((sum((abs(Life.Exp-pred))/Life.Exp))/nrow(test.data))

#MAPE=  0.006592706

## Get the predicted or fitted values
fitted(fit5)

par(mfrow=c(2,2))
plot(fit5)


