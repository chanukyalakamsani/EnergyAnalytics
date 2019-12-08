#Q2
library(fpp2)


#2a)
autoplot(plastics)
# The graph is seasonal in a upward trend


#2b)

plastics_mult <- decompose(plastics, "multiplicative")
autoplot(plastics_mult)

#2c)

# They support the inference of the result of 2a

#2d)
autoplot(plastics, series="Data") +autolayer(seasadj(plastics_mult), series="Seasonally Adjusted")+ggtitle("plastics manufacturer for five years") 

#2e)
temp_plastics <- plastics
temp_plastics[50]
temp_plastics[50] <- temp_plastics[50]+500
tmp_plastics_mult <- decompose(temp_plastics, "multiplicative")
autoplot(temp_plastics, series = "Data") +autolayer(seasadj(tmp_plastics_mult),series = "Seasonally Adjusted") +ggtitle("plastics manufacturer for five years with an outlier") 
# The outlier really effects the seasonally adjusted data.

#2f)
length(plastics)
temp_plastics <- plastics
temp_plastics[59]
temp_plastics[59] <- temp_plastics[59]+500
tmp_plastics_mult <- decompose(temp_plastics, "multiplicative")
autoplot(temp_plastics, series = "Data") +autolayer(seasadj(tmp_plastics_mult),series = "Seasonally Adjusted") +ggtitle("plastics manufacturer for five years with an outlier") 
# We can see from the plot that the effect is less when the outlier is at the end.



#3a)
head(ukcars)
autoplot(ukcars)
# The data is seasonal. Initially it had a downward trend but later i did have an upward trend.

#3b)
fit <-stl(ukcars,s.window=4)
autoplot(ukcars, series="Data") +autolayer(seasadj(fit), series="Seasonally Adjusted")

#3c)
#h =8 because 2 years
AAN_ukcars <- ukcars %>% stlf(h = 8, etsmodel = "AAN", damped = TRUE)
autoplot(stlf_ukcars)


#3d)
holt_ukcars <- ukcars%>%holt(h=8,PI=FALSE)
autoplot(holt_ukcars)

#3e)
ukcar_ets <- ets(ukcars)
# got ETS(A, N, A) model.
autoplot(forecast(ukcar_ets, h = 8))

#3f)
accuracy(AAN_ukcars)
accuracy(holt_ukcars)
accuracy(ukcar_ets)
# AAN gives the better model

#3g) 
# ANN is the optimal model among the three

#3h)
checkresiduals(AAN_ukcars)
# they are exactly normally distributed. From ACF plot we can conclude there is little information in residuals.


#4a)
head(visitors)
autoplot(visitors)
ggsubseriesplot(visitors)
ggseasonplot(visitors)
#highest number of visitors are during the month of decemeber. Lease number of visitors are during the month of may


#4b)
train <- subset(visitors,end = length(visitors) - 24)
test <-  subset(visitors,start = length(visitors) - 23)

#4c)
ggseasonplot(train)
#change of amplitude with change in seasons varies from season to season, so it is better to use multplicative method

#4d)
train_ets <- forecast(ets(train), h= 24)
autoplot(train_ets)

train_ets_boxcox<- train %>%
  ets(lambda = BoxCox.lambda(train)) %>%
  forecast(h = 24)
autoplot(train_ets_boxcox)


train_snaive <- forecast(snaive(train), h= 24)
autoplot(train_snaive)

train_ets_boxcox_stlm<- train %>%
  stlm(lambda = BoxCox.lambda(train), method= "ets") %>%
  forecast(h = 24)
autoplot(train_ets_boxcox_stlm)


#e)
accuracy(train_ets,test)
accuracy(train_ets_boxcox,test)
accuracy(train_snaive,test)
accuracy(train_ets_boxcox_stlm,test)
#snaive and train_ets_boxcox model performed alnist equally, and snaive stood out among all models

#f)
far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
