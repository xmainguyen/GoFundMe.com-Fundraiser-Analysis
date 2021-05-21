library(ggplot2)

m1 <- read.csv("C:\\Users\\wendy\\OneDrive - purdue.edu\\Purdue University\\2nd Module\\Web data analytics 59000-011\\cleaned data.csv", sep=",", header=T); # data is loaded to a frame named gmapps

m1$amount_raised = as.numeric(m1$amount_raised)
m1$goal_amount = as.numeric(m1$goal_amount)
m1$difference = as.numeric(m1$difference)
m1$donors = as.numeric(m1$donors)
m1$success = as.factor(m1$success)
m1$rate = as.numeric(m1$rate)
m1$shares = as.numeric(m1$shares)
m1$followers = as.numeric(m1$followers)

hist(m1$goal_amount);
hist(m1$amount_raised);
hist(m1$difference);

fullmodel <- lm(m1$rate~title+duration+tags+goal_amount+amount_raised+difference+success+donors+shares+followers+description, dadurta=m1)
summary(fullmodel)

#load data
data<-read.csv("cleaneddata.csv")

boxplot(data$Goal.Amount)
par(mar=c(1,1,1,1))

#remove the outlier in goal amount



# check data type
data$Success = as.factor(data$Success)
data$Tags = as.factor(data$Tags)
data$Amount.Raised = as.numeric(data$Amount.Raised)
data$Goal.Amount = as.numeric(data$Goal.Amount)
data$Difference = as.numeric(data$Difference)
data$Donors = as.numeric(data$Donors)
data$Shares = as.numeric(data$Shares)
data$Followers = as.numeric(data$Followers)

#create average donor amount
data$average <- as.numeric(data$Amount.Raised/data$Donors)
#dealing missing values in average
mean(data$average[data$average!='Inf'],na.rm=TRUE)
##'Inf'
data$average[data$average=='Inf']<-mean(data$average[data$average!='Inf'],na.rm=TRUE)
## na value
data$average<-ifelse(is.na(data$average),mean(data$average,na.rm=TRUE),data$average)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


####EDA####
library(ggplot2)
library(dplyr)


options(scipen=999)
ggplot(data,aes(Amount.Raised))+
  geom_histogram()+
  scale_x_continuous(breaks=seq(0, 100000, 100))

ggplot(data,aes(x=Goal.Amount,y=Amount.Raised))+
  geom_point()+
  coord_cartesian(xlim = c(0, 500000),ylim= c(0,500000))

#Tags Count
ggplot(data,aes(Tags))+
  geom_histogram(stat="count")+
  coord_cartesian(ylim = c(0, 120))+
  theme(axis.text.x = element_text(angle = 45))

#different amount layer comparision
data$class=cut(data$Goal.Amount,
               c(0,10000,20000,40000,60000,80000,100000,120000,140000,160000,180000,200000,Inf))
               #labels=c('0-10000','10000-20000','20000-40000','20000-40000','40000-60000','60000-80000','80000-100000',
                        #'100000-120000','120000-140000','140000-160000','160000-180000',
                        #'180000-200000','>200000')
data1=data%>%
  group_by(class)%>%
  summarise(count=n())

data2=data%>%
  group_by(class)%>%
  filter(Success=='1')%>%
  summarise(count=n())

data3<- data2$count/data1$count

cutoff<-cbind(data1,data2$count,data3)
names(cutoff) <- c('layer','count','success','rate')
cutoff

####Logistic Regression Building####
goalmodel <- lm(Success~Goal.Amount,data=data)
goalmodel$coefficients

averagemodel<-lm(Success~average,data=data)
averagemodel$coefficients

tagsmodel <-glm(data$Success~Tags,data=data)
tagsmodel$coefficients

amountmodel<- glm(Success~Goal.Amount+Amount.Raised,data=data)
amountmodel$coefficients

fullmodel <- glm(Success~Tags+Goal.Amount+Amount.Raised+Donors+Shares+Followers, data=data)
summary(fullmodel)

reducedmodel <- glm(Success~Tags+Goal.Amount+Donors+Shares+Followers, data=data)
summary(reducedmodel)

reducedmodel2 <- glm(Success~Tags+Goal.Amount+Shares+Followers, data=data,family=binomial)
summary(reducedmodel2)

#final model
reducedmodel3 <- glm(Success~Tags+Goal.Amount+Shares, data=data,family=binomial)
summary(reducedmodel3)

reducedmodel4 <- glm(Success~Tags+goalnormal+Shares, data=data,family=binomial)
summary(reducedmodel4)

reducedmodel5 <- glm(Success~Tags+goalnormal+Amount.Raised+Shares, data=data,family=binomial)
summary(reducedmodel5)

#check the multicollinearity
library(car)
vif(reducedmodel2)
vif(reducedmodel3)
vif(reducedmodel5)

