##installing and loading libraries 
install.packages("readxl")
install.packages("forecast")
install.packages("fpp")
install.packages("smooth")
library(forecast)
library(readxl)
library(fpp)
library(smooth)

##importing the dataset
airline<-read_excel("C:\\Users\\91755\\Desktop\\Assignment\\forecasting\\Airlines+Data.xlsx")
head(airline)
str(airline)
airline$Month<-as.Date(airline$Month)
windows()
plot(airline$Passengers,type = "o")

#creating dummy variables

month <- data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(month) <- month.abb
View(month)
airline<- cbind(airline,month)
View(airline)


## data split into train and test set
train<-airline[1:84,]

test<-airline[85:96,]
head(train)

##################### auto.arima method  ##########################

library(tseries)
airline_ts <- as.ts(airline$Passengers)
airline_ts <- ts(airline_ts,start = c(1949,1),end = c(1953,12),frequency = 12)
class(airline_ts)
start(airline_ts)
end(airline_ts)
sum(is.na(airline_ts))
summary(airline_ts)


decompdata1 <- decompose(airline_ts,"multiplicative")
plot(decompdata1)
cycle(airline_ts)


#################### model ######################
newmodel1 <- auto.arima(airline_ts,ic = "aic",trace = T)
plot.ts(newmodel1$residuals)


# verifying p,d,q values using acf and pacf
acf(newmodel1$residuals)                        #q=0
pacf(newmodel1$residuals)                       #p=1


#forecasting the model
forecasting1 <- forecast(newmodel1,level = c(95),h=5*12)
plot(forecasting1)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2
hw_a<-HoltWinters(airline_ts,alpha = 0.2,beta = F, gamma = F) # simple exponential smooting#
hw_a
hwa_pred<-forecast(hw_a)
hwa_pred<-data.frame(predict(hw_a,n.ahead=12))
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,test$Passengers)*100
hwa_mape                ##50.95162

# with alpha = 0.2, beta = 0.1
hw_b<-HoltWinters(airline_ts,alpha = 0.2,beta = 0.1, gamma = F)
hw_b
hwb_pred<-forecast(hw_b)
hwb_pred<-data.frame(predict(hw_b,n.ahead=12))
plot(forecast(hw_b,h=12))
hwb_mape<-MAPE(hwb_pred$fit,test$Passengers)*100
hwb_mape              ##39.60003

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hw_c<-HoltWinters(airline_ts,alpha = 0.2,beta = 0.1, gamma = 0.1)
hw_c
hwc_pred<-forecast(hw_c)
hwc_pred<-data.frame(predict(hw_c,n.ahead=12))
plot(forecast(hw_c,h=12))
hwc_mape<-MAPE(hwc_pred$fit,test$Passengers)*100
hwc_mape             ##36.19073

# Without optimum values 
hw_d<-HoltWinters(airline_ts,beta = F, gamma = F)
hw_d
hwd_pred<-forecast(hw_d)
hwd_pred<-data.frame(predict(hw_d,n.ahead=12))
hwd_pred
plot(forecast(hw_d,h=12))
hwd_mape<-MAPE(hwd_pred$fit,test$Passengers)*100
hwd_mape            ##63.30959

hw_e<-HoltWinters(airline_ts, gamma = F)
hw_e
hwe_pred<-forecast(hw_e)
hwe_pred<-data.frame(predict(hw_e,n.ahead=12))
hwe_pred
plot(forecast(hw_e,h=12))
hwe_mape<-MAPE(hwe_pred$fit,test$Passengers)*100
hwe_mape           ##51.54651

hw_f<-HoltWinters(airline_ts)
hw_f
hwf_pred<-forecast(hw_f)
hwf_pred<-data.frame(predict(hw_f,n.ahead=12))
hwf_pred
plot(forecast(hw_f,h=12))
hwf_mape<-MAPE(hwf_pred$fit,test$Passengers)*100
hwf_mape        ##49.75166

############################## STOP HERE ###############################

df_mape<-data.frame(c("hwa_mape","hwb_mape","hwc_mape","hwd_mape","hwe_mape","hwf_mape"),c(hwa_mape,hwb_mape,hwc_mape,hwd_mape,hwe_mape,hwf_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
