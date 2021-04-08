dm<-read.csv("D:\\Jigsaw Academy Course\\Predictive Analytics\\T1. Predictive Analytics - Linear Regression Models\\DirectMarketing-2.csv")
library(ggplot2)
library(dplyr)
library(car)
library(gains)

library(irr)
library(lattice)
library(caret)
#exploring data and  checking relationship with AmountSpent

#Age
plot(dm$Age,dm$AmountSpent,col="red")

dm$Age1<-ifelse(dm$Age!="Young","Middle-Old",as.character(dm$Age))
dm$Age1<-as.factor(dm$Age1)
summary(dm$Age1)
plot(dm$Age1,dm$AmountSpent,col="red")

#Gender
summary(dm$Gender)
plot(dm$Gender,dm$AmountSpent,col="red")

#OwnHome
summary(dm$OwnHome)
plot(dm$OwnHome,dm$AmountSpent,col="red")

#Married
summary(dm$Married)
plot(dm$Married,dm$AmountSpent,col="red")

#Location
summary(dm$Location)
plot(dm$Location,dm$AmountSpent,col="red")

#Salary
summary(dm$Salary)
plot(dm$Salary,dm$AmountSpent,col="red")

#Children
summary(dm$Children)
dm$Children <-as.factor(dm$Children)
plot(dm$Children,dm$AmountSpent,col="red")


#History
summary(dm$History)
tapply(dm$AmountSpent, dm$History, mean)

ind<-which(is.na(dm$History))
mean(dm[ind,"AmountSpent"])

mean(dm[which(is.na(dm$History)), "AmountSpent"])

#create a category called missing
dm$History1<-ifelse(is.na(dm$History), "Missing", as.character(dm$History))
dm$History1<-as.factor(dm$History1)

plot(dm$History1,dm$AmountSpent,col="red")

#Catalogs
summary(dm$Catalogs)

dm$Catalogs<-as.factor(dm$Catalogs)
plot(dm$Catalogs,dm$AmountSpent,col="red")


data1<-dm[,-c(1,8)]

mod1<-lm(AmountSpent~.,data=data1)
summary(mod1)

mod2<-lm(formula = AmountSpent~ Gender + Location + Salary + Children + Catalogs +History1, data=data1)
summary(mod2)

#creating dummy variables
#History Missing and Gender Male
data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(data1$History1=="Missing",1,0)
data1$Med_d<-ifelse(data1$History1=="Medium",1,0)
data1$High_d<-ifelse(data1$History1=="High",1,0)
data1$Low_d<-ifelse(data1$History1=="Low",1,0)

mod3<-lm(formula = AmountSpent~ Male_d + Missing_d + Med_d + High_d + Location + Salary + Children + Catalogs, data=data1)
summary(mod3)

mod4<-lm(formula = AmountSpent~ Missing_d + High_d + Location + Salary + Children + Catalogs, data=data1)
summary(mod4)

#assumption checks

#Assumption of normality
hist(mod4$residuals)
qqPlot(mod4$residuals)

#multicollinearity check
vif(mod4)

#constant variance check
plot(mod4$fitted.values,mod4$residuals) #funnel pattern

mod5<-lm(formula = log(AmountSpent)~ Missing_d + High_d + Location + Salary + Children + Catalogs, data=data1)
summary(mod5)
hist(mod5$residuals)
qqPlot(mod5$residuals)
vif(mod5)
plot(mod5$fitted.values,mod5$residuals)

predicted<-mod5$fitted.values
actual<-log(data1$AmountSpent)
dat<-data.frame(predicted,actual)

p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="black")


# for logistic regressions
data<-dm
data<-data%>%mutate(Target=ifelse(data$AmountSpent>mean(data$AmountSpent),1,0))
data<-data%>%select(-AmountSpent)
summary(data)

data$Children<-as.factor(data$Children)
data$Catalogs<-as.factor(data$Catalogs)
data$History1<-ifelse(is.na(data$History), "Missing", as.character(data$History))
data$History1<-as.factor(data$History1)
data<-data%>%select(-History)



#splitting into test and training set
set.seed(200)
index<-sample(nrow(data),0.70*nrow(data),replace = F)
train<-data[index,]
test<-data[-index,]

#Build the first model using all variables
mod1<-glm(Target~.,data=train,family="binomial")
summary(mod1)


#Check stepwise procedure to find significant variale and place
step(mod1,direction = "both")

mod2<-glm(formula = Target ~ Location + Salary + Children + Catalogs + 
            History1, family = "binomial", data = train)
summary(mod2)

#creating dummies to remove insignificant variable

train$Hist_Med_d<-ifelse(train$History1=="Medium",1,0)
test$Hist_Med_d<-ifelse(test$History1=="Medium",1,0)

mod3<-glm(formula = Target ~ Location + Salary + Children + Catalogs + 
            Hist_Med_d, family = "binomial", data = train)
summary(mod3)

#test the model

pred<-predict(mod3,type = "response", newdata = test)
head(pred)

table(data$Target)/nrow(data)
pred<-ifelse(pred>=0.399,1,0)
pred<-as.factor(pred)
test$Target<-as.factor(test$Target)
kappa2(data.frame(test$Target,pred))

confusionMatrix(pred,test$Target,positive = "1")
summary(pred)
summary(test$Target)