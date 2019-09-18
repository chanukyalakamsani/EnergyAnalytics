library(fpp2)
View(visnights)
autoplot(visnights[,"QLDMetro"])
x <- ts(visnights[,"QLDMetro"],start = 1998, frequency=4) 
x


#1a)
train1 <- window(x,end = c(2015,4))
train2 <- window(x,end = c(2014,4))
train3 <- window(x,end = c(2013,4))


#1b)
fc1 <- snaive(train1, h = 4)
autoplot(fc1)
fc2 <- snaive(train2, h = 8)
autoplot(fc2)
fc3 <- snaive(train3, h = 12)
autoplot(fc3)

#1c)
test1 <- window(x,start=c(2016,1), end= c(2016,4))
fc1$mean
accuracy(test1,fc1$mean)
test2 <- window(x,start=c(2015,1), end= c(2016,4))
accuracy(test2,fc2$mean)
test1 <- window(x,start=c(2014,1), end= c(2016,4))
accuracy(test3,fc3$mean)



#1d)





#2a)
#View(dowjones)

autoplot(dowjones)

#2b)
#forecast using drift method
rwf1<- rwf(dowjones,drift = TRUE)
autoplot(rwf(dowjones,drift = TRUE))

#2c)
x1 <- unlist(dowjones[1])
x1
y1 <- unlist(dowjones[78])
y1
autoplot(rwf1) + geom_line(aes(x = c(1, 78), y = dowjones[c(1, 78)]), colour = "maroon")

#2d)
plot(rwf1$residuals)
naive1 <- naive(dowjones)
naive1
plot(naive1$residuals)
snaive1 <- snaive(dowjones)
plot(snaive1$residuals)
ggAcf(snaive1$residuals)
ggAcf(naive1$residuals)
ggAcf(rwf1$residuals)
sum(snaive1$residuals[2:length(snaive1$residuals)])
sum(naive1$residuals[2:length(naive1$residuals)])
sum(rwf1$residuals[2:length(rwf1$residuals)])
#drift performs the best among all forecasting methods as we are getting least error.


#3a)
#a)
autoplot(ibmclose)
#ggseasonplot(ibmclose)
#data is not seasonal
#ggsubseriesplot(ibmclose)
gglagplot(ibmclose)
ggAcf(ibmclose)
#data looks and good and there is no white noise in the data

#3b)
length(ibmclose)
train <- ibmclose[1:300]
test <- ibmclose[301:369]


#3c)

# seasonal naive
snaive2 <- snaive(train, h = 69)
accuracy(test,snaive2$mean)

# naive method
naive2 <- naive(train, h = 69)
accuracy(test,naive2$mean)

# drift method
rwf2 <- rwf(train, h = 69, drift = TRUE)
accuracy(test,rwf2$mean)

# drift method because of least RMSE error


#3d)
ggAcf(snaive2$residuals)
ggAcf(naive2$residuals)
ggAcf(rwf2$residuals)
# All did perform equally


#4)
autoplot(hsales)


#b)
hsal <- ts(hsales, start = 1973, frequency=12)
hsal
train <- subset(hsales, end = length(hsales)-24 )
test <- subset(hsales, start = length(hsales)-23)

#c)
# seasonal naive
snaive3 <- snaive(train, h = 23)
accuracy(test,snaive3$mean)

# naive method
naive3 <- naive(train, h = 23)
accuracy(test,naive3$mean)

# drift method
rwf3 <- rwf(train, h = 23, drift = TRUE)
accuracy(test,rwf3$mean)

# seasonal naive method because of least RMSE error

#d)
ggAcf(snaive3$residuals)

# Seasonal naive doesnot depict the white noise
