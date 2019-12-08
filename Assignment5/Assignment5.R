library(fpp2)
library(kableExtra)
#1
#a)
usmelc_ma_trend <- ma(usmelec, order =12)
autoplot(usmelec)+autolayer(usmelc_ma_trend)
# Initally it is increasing, but in the end we can see it stopped increasing.

#b)
lambda <- BoxCox.lambda(usmelec)
lambda
# Yes data needs transformation.

#c)
#from the graph we saw that data is not stationary
#the acf drops to zero for stationary time series
acf(usmelec)
#from the graph we can say the data is not stationary
ndiffs(usmelec)
nsdiffs(usmelec)
#require one seasonal differencing to make data stationary

#d)
#we see positive auto correlation for all the data
usmelec %>% diff(lag=12) %>% ggtsdisplay()
# we can say data is stationary, but we can see some seasonality so we take one more difference
usmelec %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
# we see a significant spike at lag1 in ACF and PACF

usmelec %>%
	Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
	residuals() %>% ggtsdisplay()
# we see spikes at lag1, almost significant spikes in lag 2, indication some additional non-seasonal terms to be included

usmelec %>%
	Arima(order=c(1,0,1), seasonal=c(0,1,1)) %>%
	residuals() %>% ggtsdisplay()

usmelec %>%
	Arima(order=c(1,0,2), seasonal=c(0,1,1)) %>%
	residuals() %>% ggtsdisplay()
#this looks like a goo model among the selected one's

#lets try auto arima
auto.arima(usmelec)

fit1 <- Arima(usmelec, order=c(0,1,1), seasonal=c(0,1,1))
checkresiduals(fit1)

fit2 <- Arima(usmelec, order=c(0,1,2), seasonal=c(0,1,1))
checkresiduals(fit2)

fit3 <- Arima(usmelec, order=c(1,0,2), seasonal=c(0,1,1))
checkresiduals(fit3)

fit1$aic
fit2$aic
fit3$aic
# we get best value for out second model

#e)

#second model provides the least i


#2)
#a)
lambda_mcopper <- BoxCox.lambda(mcopper)
lambda_mcopper

#b)
fit <- auto.arima(mcopper, lambda = lambda)
fit
autoplot(fit$residuals)
#Increasing trend
autoplot(diff(mcopper))
#data is stationary with signle differnecing

#c)

mcopper_fit1 <- Arima(mcopper, lambda = lambda_mcopper, order = c(0,1,1))
mcopper_fit1

autoplot(mcopper)+autolayer(mcopper_fit1$fitted)

#the model perfectly fits.

mcopper_fit2 <- Arima(mcopper, lambda = lambda_mcopper, order = c(1,0,1), include.drift = TRUE)


#d)

accuracy(mcopper_fit1)
accuracy(mcopper_fit2)

#Drift models works well with the data with less RMSE

#e)
fit1%>%
	forecast() %>%
	autoplot()

#Drift model forecast
fit2%>%
	forecast() %>%
	autoplot()

# Yes they look reasonable

#f)
mcopper_ets <- forecast(ets(mcopper))
autoplot(mcopper_ets)



#3)
library(caret)
library(ISLR)
#Weekly$Direction
train_data <- Weekly[Weekly$Year <= 2008,]
test_data <- Weekly[Weekly$Year > 2008,]

logreg <- glm(Direction ~ Lag2, data = train_data, family = 'binomial')
pred <- predict(logreg, newdata = test_data, type = 'response')
class_prediction <- ifelse(pred > 0.50,"Down","Up")
class_prediction <- as.factor(class_prediction)
test_output   <- as.factor(test_data$Direction)
confusionMatrix(class_prediction,test_output)


#b)
library(class)

knn_pred <- knn(train = data.frame(train_data$Lag2),
								test = data.frame(test_data$Lag2),
								cl = train_data$Direction, k = 5)
confusionMatrix(knn_pred,test_output)


#c)
values  <- c(0,0,0,0,0,0,0,0,0)
x <- c(1,2,3,4,5,6,7,8,9)
for (val in x) {
	knn_pred <- knn(train = data.frame(train_data$Lag2),
									test = data.frame(test_data$Lag2),
									cl = train_data$Direction, k = val)
	cm <- confusionMatrix(knn_pred,test_output)
	acc <- cm$overall['Accuracy']
	print(acc)
}

#for k =4 we are getting maximum accuracy


#d)
library(e1071)
classifier = svm(formula = Direction ~ Lag2,
								 data = train_data,
								 type = 'C-classification',
								 kernel = 'linear')
pred <- predict(classifier, newdata = test_data)
confusionMatrix(pred,test_output)

