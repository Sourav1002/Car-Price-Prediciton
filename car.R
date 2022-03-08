#importing llibrary
library(ggplot2)
library(dplyr)
library(corrplot)
library(caTools)
library(caret)

#import dataset
data<-read.csv("car_data.csv")
data= na.omit(data)  #omit null values
summary(data)

#find age of the car
data['age'] = 2021 - data[3]

#drop unwanted columns
data[3]<-NULL
data[2]<-NULL
data[1]<-NULL
summary(data)

#HP
q1<- quantile(data$Engine.HP,0.25)
q3<-quantile(data$Engine.HP,0.75)
iqr<-IQR(data$Engine.HP)
no_outliers<- subset(data,data$Engine.HP >(q1 - 1.5*iqr) &  data$Engine.HP <(q3 + 1.5*iqr))
dim(no_outliers)

#MSRP
q1<- quantile(data$MSRP,0.25)
q3<-quantile(data$MSRP,0.75)
iqr<-IQR(data$MSRP)
no_outliers<- subset(data,data$MSRP >(q1 - 1.5*iqr) &  MSRP <(q3 + 1.5*iqr))
dim(no_outliers)


                
glimpse(data)   
 
#converting as integer
data$Engine.Fuel.Type=as.integer(as.factor(data$Engine.Fuel.Type))
data$Engine.Cylinders=as.integer(as.factor(data$Engine.Cylinders))
data$Transmission.Type=as.integer(as.factor(data$Transmission.Type))
data$Driven_Wheels=as.integer(as.factor(data$Driven_Wheels))
data$Vehicle.Size=as.integer(as.factor(data$Vehicle.Size))
data$Market.Category=as.integer(as.factor(data$Market.Category))
data$Vehicle.Style=as.integer(as.factor(data$Vehicle.Style))   
data$age=as.integer(as.factor(data$age))      

#corelation
m=cor(data)
corrplot(m)

#drop unwanted column
data$Engine.Fuel.Type<-NULL
data$Driven_Wheels<-NULL
data$Number.of.Doors<-NULL
data$Vehicle.Size<-NULL
data$Vehicle.Style<-NULL
data$Popularity<-NULL

glimpse(data)

#Plot
ggplot(data, aes(x = Engine.HP, y = MSRP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("HP") +
  ylab("Price") +
  ggtitle("Price of Car by HP")

ggplot(data, aes(x = Transmission.Type, y = MSRP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Transmission") +
  ylab("Price") +
  ggtitle("Price of Car by Transmission")


summary(data)

set.seed(123)
sample_Set<- sample.split(data,SplitRatio= .7)
datatrain <-subset(data,sample_Set==TRUE)
datatest<-subset(data,sample_Set==FALSE)

# Regression

model= lm(MSRP ~ ., data=datatrain)
summary(model)

res=residuals(model)
res=as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5)
plot(model)

datatest$predicted.MSRP <- predict(model,datatest)

error<-datatest$MSRP - datatest$predicted.MSRP
rmse<-sqrt(mean(error)^2)/1000
