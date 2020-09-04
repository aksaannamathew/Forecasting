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
plastic<-read.csv("C:\\Users\\91755\\Desktop\\Assignment\\forecasting\\PlasticSales.csv")
head(plastic)
str(plastic)

windows()
plot(plastic$Sales,type = "o")

#creating dummy variables

month <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(month) <- month.abb
View(month)
plasticsale<- cbind(plastic,month)
View(plasticsale)

plasticsale["logsale"]<-log(plasticsale["Sales"])
plasticsale['t']<-1:60
plasticsale["t_square"]<-plasticsale['t']*plasticsale['t']

## data split into train and test set
train<-plasticsale[1:54,]

test<-plasticsale[54:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)                
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 252.36 and Adjusted R2 Vaue 31%

######################### Exponential #################################

expo_model<-lm(logsale~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 256.9441 and Adjusted R2 30.65

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 250.891 and Adjusted R2 31.09%

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 186.6768  and Adjusted R square 68.10%

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 178.5941 and Adjusted R2 97.24

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 246.8046 and Adjusted R2 - 98.16%

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(logsale~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 192.7751 and Adjusted R sqaure 69.6%

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(logsale~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 220.5664 and Adjusted R2 98.05%

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality trend has least RMSE value

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plasticsale)
new_model_pred<-data.frame(predict(Add_sea_Linear_model,newdata=plasticsale,interval='predict'))
new_model_fin <- Add_sea_Linear_model$fitted.values

View(new_model_fin)

month <- as.data.frame(plasticsale$Sales)

Final <- as.data.frame(cbind(month,plasticsale$Sales,new_model_fin))
colnames(Final) <-c("month","Sales","New_Pred_Value")
plot(Final$Sales,type="o") 
plot(plastic$Sales,type = "o",col="green")

#----------------------------------------X------------------------------------#
##################### auto.arima method  ##########################

library(tseries)
plastic_ts <- as.ts(plastic$Sales)
plastic_ts <- ts(plastic_ts,start = c(1949,1),end = c(1953,12),frequency = 12)
class(plastic_ts)
start(plastic_ts)
end(plastic_ts)
sum(is.na(plastic_ts))
summary(plastic_ts)

decompdata1 <- decompose(plastic_ts,"multiplicative")
plot(decompdata1)
cycle(plastic_ts)

#################### model ######################
newmodel1 <- auto.arima(plastic_ts,ic = "aic",trace = T)
plot.ts(newmodel1$residuals)

# verifying p,d,q values using acf and pacf
acf(newmodel1$residuals)                        #q=0
pacf(newmodel1$residuals)                       #p=1

#forecasting the model
forecasting1 <- forecast(newmodel1,level = c(95),h=5*12)
plot(forecasting1)