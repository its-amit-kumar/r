########################### Lab 01 #########################################

rm(list=ls())
library(dplyr)
library(Metrics)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))
#setting  the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

correlation<-cor.test(train$wt,train$mpg)

print(correlation)

plot(train$wt,train$mpg,xlab = "Wt",ylab = "mpg",main="Wt VS Displacement")
##Linear model

lmodel<-lm(mpg~wt,data=train)
abline(lmodel,col="red")

summary(lmodel)

predicted<-predict(lmodel,data=test)
mae(test$mpg,predicted)

##############

rm(list=ls())
library(dplyr)

setwd("C:/Users/hp/Desktop/Winter Semester 21-22/EDA/Lab/01")
data<-read.csv('data.csv')
## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))
#setting  the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
correlation<-cor.test(train$Height,train$Weight)

print(correlation)

plot(train$Weight,train$Height,xlab = "Weight",ylab = "Height",main="Weight vs Height")

##Linear model
lmodel<-lm(Height~Weight,data=train)
abline(lmodel,col="red")
summary(lmodel)


predicted<-predict(lmodel,data=test)
mae(test$Height,predicted)

########################### Lab 02 #########################################

setwd("C:/Users/hp/Desktop/Winter Semester 21-22/EDA/Lab/02")

gold <- read.csv("gold.csv")
library(forecast)
library(tseries)
view(gold)
goldts<-ts(gold$Price, start = min(gold$Month), end = max(gold$Month), frequency = 1)
class(goldts)
plot(goldts)
acf(goldts)
pacf(goldts)
adf.test(goldts) # stationary only if p value <0.05
# To make it stationary, differentiate
goldmodel=auto.arima(goldts, ic='aic', trace = TRUE)
goldf=forecast(goldmodel, level=c(95), h=24)
goldf
plot(goldf)

#####################

setwd("C:/Users/hp/Desktop/Winter Semester 21-22/EDA/Lab/02")

gdp <- read.csv("gdp.csv")
library(forecast)
library(tseries)
view(gdp)
gdpts<-ts(gdp$GDP_gr, start = min(gdp$Year), end = max(gdp$Year), frequency = 1)
class(gdpts)
plot(gdpts)
acf(gdpts)
pacf(gdpts)
adf.test(gdpts) # stationary only if p value <0.05
# To make it stationary, differentiate
gdpmodel=auto.arima(gdpts, ic='aic', trace = TRUE)
gdpf=forecast(gdpmodel, level=c(95), h=24)
gdpf
plot(gdpf)


########################### Lab 03 #########################################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\03")
df=read.csv("weatherHistory2016.csv")
head(df)
library(dplyr)
library(GGally)
a=sample_n(df,200)
a <- a[,c(4:9)]
head(a)
cor.test(a$Temperature..C.,a$Apparent.Temperature..C.)
cor.test(a$Temperature..C.,a$Humidity)
cor.test(a$Temperature..C.,a$Wind.Speed..km.h.)
cor.test(a$Temperature..C.,a$Wind.Bearing..degrees.)
cor.test(a$Temperature..C.,a$Pressure..millibars.)
cor.test(a$Temperature..C.,a$Visibility..km.)
cor.test(a$Temperature..C.,a$Loud.Cover)
ggcorr(a, label = TRUE)
lmodel=lm(a$Temperature..C.~a$Apparent.Temperature..C.+a$Humidity+a$Visibility..km.)
summary(lmodel)

#########################

library(forecast)
library(tseries)
data<-ts(df$Temperature..C.,start = as.Date("2016-10-01"),end = as.Date("2016-12-31"),frequency = 24)
plot(data)
acf(data)
pacf(data)
adf.test(data)
model=auto.arima(data,ic="aic",trace=TRUE)
f=forecast(model,level=c(95),h=24)
f
plot(f)
accuracy(model)


########################### Lab 04 #########################################

# To clear the environment
rm(list=ls())

setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\04\\")
data <- read.csv("color-anova-example.csv")

library(dplyr) # To group the data


group_by(data,color) %>% summarise(count = n(),mean = mean(response, na.rm = TRUE))

ANOVA <- aov(response~color, data = data)
summary(ANOVA)
TukeyHSD(ANOVA)


########################### Lab 05 #########################################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\05")
mydata<-read.csv("Social_Network_Ads.csv")
library(caTools)
splitd<-sample.split(mydata,SplitRatio = 0.8)
train=subset(mydata,splitd=="TRUE")
test=subset(mydata,splitd=="FALSE")
train
mydata$Gender<-as.factor(mydata$Gender)
mydata$Purchased<-as.factor(mydata$Purchased)
mymodel <- glm(Purchased ~ Age+Gender+EstimatedSalary, data=train,
               family='binomial')
summary(mymodel)
restrain<-predict(mymodel,train,type='response')
plot(restrain)
restest<-predict(mymodel,test,type='response')
plot(restest,col='red')
par(new=TRUE)
plot(test$Purchased)
cfmatrix<-table(Act=test$Purchased, pred=restest>0.5)
cfmatrix
Acc=(cfmatrix[[1,1]]+cfmatrix[[2,2]])/sum(cfmatrix)
Acc
plot(restest)


########################### Lab 06 #########################################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\06")
wdbc<-read.table(file.choose(),sep=',')
view(wdbc)
wdbc<-wdbc[,-1]
mynorm<-function(x){((x-min(x))/(max(x)-min(x)))}
mydata<-as.data.frame(lapply(wdbc[,-1], mynorm))
summary(wdbc[,2:5])
summary(mydata[,1:4])
train<-mydata[1:400,]
test<-mydata[401:569,]
library(class)
pred<-knn(train,test,wdbc[1:400,1],k=21)
cf<-table(pred,wdbc[401:569,1])
cf
acc=(cf[[1,1]]+cf[[2,2]])/sum(cf)
acc


########################### Lab 07 #########################################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\07\\Data")
data1<-read.csv("iris.csv")
View(data1)
df<-scale(data1)
fit<-kmeans(df,centers=2)
fit$cluster
fit$size
fit$withinss
fit$tot.withinss
Kmax<-15
wcss<-rep(NA,Kmax)
nClust<- list()
for(i in 1:Kmax){
  fit<-kmeans(df,i)
  wcss[i]<-fit$tot.withinss
  nClust[[i]]<-fit$size
}
plot(1:Kmax,wcss,type="b",pch=19)
fit<-kmeans(df,centers=3)
fit$cluster
fit$size
fit$center
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")
fviz_cluster(fit, data1)
library(cluster)
fitm <- pam(df, 3, metric = "manhattan")
fitm
fitm$medoids
fviz_cluster(fitm, data1)

###########################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\07\\Data")
data2<-read.csv("USArrests.csv")
view(data2)
data2<-data2[,-1]
df1<-scale(data2)
fit1<-kmeans(df1,centers=2)
fit1$cluster
fit1$size
fit1$withinss
fit1$tot.withinss
Kmax1<-15
wcss1<-rep(NA,Kmax1)
nClust1<- list()
for(i in 1:Kmax1){
  fit1<-kmeans(df1,i)
  wcss1[i]<-fit1$tot.withinss
  nClust1[[i]]<-fit1$size
}
plot(1:Kmax1,wcss1,type="b",pch=19)
fit1<-kmeans(df1,centers=3)
fit1$cluster
fit1$size
fit1$center
library(factoextra)
fviz_nbclust(df1, kmeans, method = "wss")
fviz_cluster(fit1, data2)
library(cluster)
fitm1 <- pam(df1, 3, metric = "manhattan")
fitm1
fitm1$medoids
fviz_cluster(fitm1, data2)


########################### Lab 08 #########################################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\08")
data <- read.csv("iris.csv",row.names=1)
View(data)
df <- scale(data)
View(df)
ed <- dist(df, method = 'euclidean')
hierClust <- hclust(ed, method = 'complete')
plot(hierClust)
cluster <- cutree(hierClust, k = 4)
cluster
rect.hclust(hierClust, k = 4, border = 2:4)

########################

rm(list=ls())
setwd("C:\\Users\\hp\\Desktop\\Winter Semester 21-22\\EDA\\Lab\\08")
data <- read.csv("USArrests.csv",row.names=1)
View(data)
df <- scale(data)
View(df)
ed <- dist(df, method = 'euclidean')
hierClust <- hclust(ed, method = 'complete')
plot(hierClust)
cluster <- cutree(hierClust, k = 4)
cluster
rect.hclust(hierClust, k = 4, border = 2:4)


########################### Lab 09 #########################################

rm(list=ls())
gd<-function(x,y,m,c,alpha,conv_thr,iter){
  iterations=0
  Lf=0
  while(iterations<=iter){
    y_pred=m*x+c
    Lf_new=0.5*(sum(y_pred-y)^2)
    m=m-alpha*sum((y_pred-y)*x)
    c=c-alpha*sum(y_pred-y)
    if(abs(Lf-Lf_new)<conv_thr){
      break;
      }
    Lf=Lf_new
    iterations=iterations+1
    }
  return(paste('Optimum Slope',m,"Optimum Intercept",c,"Number of iterations",iterations,"Loss function",Lf))
  }
data<-mtcars
gd(data$wt,data$mpg,32,-0.2,0.005,0.0001,10000)
reg<-lm(data$mpg~data$wt)
reg

########################### Lab 10 #########################################

rm(list=ls())
gd<-function(x,y,m,c,alpha,conv_thr,iter){
  iterations=0
  Lf=0
  while(iterations<=iter){
    y_pred=m*x+c
    Lf_new=0.5*(sum(y_pred-y)^2)
    m=m-alpha*sum((y_pred-y)*x)
    c=c-alpha*sum(y_pred-y)
    if(abs(Lf-Lf_new)<conv_thr){
      break;
      }
    Lf=Lf_new
    iterations=iterations+1
    }
  return(paste('Optimum Slope',m,"Optimum Intercept",c,"Number of iterations",iterations,"Loss function",Lf))
  }
data<-mtcars
gd(data$wt,data$mpg,32,-0.2,0.005,0.0001,10000)
reg<-lm(data$mpg~data$wt)
reg