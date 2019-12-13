#######################   FINAL EXAM     ############################ 
library(fpp2)

# 1)

#a) 
autoplot(bicoal)
# From the plot we can see that there is not particular trend or seasonlity.

#b)
 # The give equation : 𝑦𝑡 = 𝑐 + 𝜑1 𝑦𝑡−1 + 𝜑2 𝑦𝑡−2 + 𝜑3 𝑦𝑡−3 +𝜑4 𝑦𝑡−4 + 𝑒𝑡
 #  p : the number of autoregressive terms, from the above equation we can say p value is equal to 4
 #  d : the number of nonseasonal differences, from the the above equation we see that there is no difference taken into consideration.
 #  q : the number of moving average terms whic can also be said as number of lagged forecast errors, from the given equation we see that none of lagged forecast errors are taken into consideration
 # so the final (p,d,q) values for the above equation is (4,0,0).

#c)

##
ggAcf(bicoal)

# There decreasing correlation till lag 12. The plot depicts more of a sinusoidal wave

ggPacf(bicoal)

# There is lage first and fourth spikes

#d)

c = 162.00
pi1 = 0.83
pi2 = -.34
pi3 = 0.55
pi4 = -.38

y_1968 = 545
y_1967 = 552
y_1966 = 534
y_1965 = 512
  
  
  
 y_1969 = c + (pi1*y_1968) + (pi2*y_1967) + (pi3*y_1966) + (pi4*y_1965)
 y_1970 = c + (pi1*y_1969) + (pi2*y_1968) + (pi3*y_1967) + (pi4*y_1966)
 y_1971 = c + (pi1*y_1970) + (pi2*y_1969) + (pi3*y_1968) + (pi4*y_1967)

print("y_1969" =  y_1969)
print("y_1970" =  y_1970)
print("y_1971" =  y_1971)

 
 
 # e)
 
 fitARIMA <- arima(bicoal, order=c(4,0,0))
 predict(fitARIMA,n.ahead = 3)
 fitARIMA$coef

 
 # The values obtained are [527.6291,517.1923,503.8051]
 
 # They slightly greater than previously predicted ones because of larger intercept. 
 
 
 
 ##2)
 
 # a) 
 autoplot(usgdp)
# from the plot we can see upward trend and seasonality.

 lambdaa = BoxCox.lambda(usgdp)
 transformed_usgdp = BoxCox(usgdp,lambdaa)
 autoplot(transformed_usgdp)

# After trasformation the data seems to be more linear than data without transformation.

  
#b)
 
autoar_usgdp <- auto.arima(usgdp, lambda = lambdaa)
autoar_usgdp

# auto_arima  gave (2,1,0) is the best model.



#c)

# first order differencing 
ndiffs(BoxCox(usgdp,lambdaa))
# requires 1 differencing 

diff_transformed_usgdp <- diff(transformed_usgdp)

ggAcf(diff_transformed_usgdp)
# From the above plot, we can see the spikes at lag 1, lag2 and lag 12

ggPacf(diff_transformed_usgdp)
# From the above plot, we can see the spikes at lag 1 and lag 12

autoplot(diff_transformed_usgdp)
# The data seems to be stationary

#(1,1,0)
arima_usgdp_110 <- Arima( usgdp, lambda = lambdaa, order = c(1, 1, 0),include.drift = FALSE)
autoplot(usgdp)+autolayer(arima_usgdp_110$fitted)

#include Drift
arima_usgdp_110_true <- Arima( usgdp, lambda = lambdaa, order = c(1, 1, 0), include.drift = TRUE)
autoplot(usgdp)+autolayer(arima_usgdp_110_true$fitted)

# from the graphs both are fitting properly

#(2,1,0)

arima_usgdp_210 <- Arima( usgdp, lambda = lambdaa, order = c(2, 1, 0),include.drift = FALSE)
autoplot(usgdp)+autolayer(arima_usgdp_210$fitted)

#include Drift
arima_usgdp_210_true <- Arima( usgdp, lambda = lambdaa, order = c(2, 1, 0), include.drift = TRUE)
autoplot(usgdp)+autolayer(arima_usgdp_210_true$fitted)


#d)
accuracy(arima_usgdp_110)
accuracy(arima_usgdp_110_true)
accuracy(arima_usgdp_210)
accuracy(arima_usgdp_210_true)

# from the above models we can see that  when we include drife we are getting lower RMSE and MAE.

checkresiduals(arima_usgdp_110)
checkresiduals(arima_usgdp_110_true)
checkresiduals(arima_usgdp_210)
checkresiduals(arima_usgdp_210_true)

# we can infer that all the residuals are white noise.
# we are getting better accuracy for arima_usdp_210_true model because of low MAE, RMSE.
# so arima_usdp_210_true gave better results than rest of the models.


# e)
autoplot(usgdp)+autolayer(forecast(arima_usgdp_210_true,h=15))
autoplot(usgdp)+ autolayer(forecast(ets(usgdp),h=15))

# both model look almost same, from the model we can see that ets is more linear and where as forecasting using arima model is taking consideration of lags


##for understanding the results
train <- usgdp[1:230]
test  <- usgdp[230:237]


arima_usgdp_210_true_train <- Arima( train, lambda = lambdaa, order = c(2, 1, 0), include.drift = TRUE)
forecast(arima_usgdp_210_true_train,h=7)
forecast(ets(usgdp),h=7)

# from the above results we can say that arima model performed better than ets.


# 3)

##a) 

summary(books)

paperback = books[,"Paperback"] 
Hardcover = books[,"Hardcover"]

# forecast for four days paperback:
results_paperback_holt <- holt(paperback, h = 4)
results_paperback_holt
# forecast for four days Hardcover:
results_hardcover_holt <- holt(Hardcover, h = 4)
results_hardcover_holt


## b) 
results_paperback_ses <- ses(paperback, h= 4)
results_paperback_ses
results_hardcover_ses <- ses(Hardcover, h= 4)
results_hardcover_ses

accuracy(results_hardcover_holt)
accuracy(results_hardcover_ses)
accuracy(results_paperback_ses)
accuracy(results_paperback_holt)

#plots
autoplot(paperback)
autoplot(Hardcover)


## from the above results we see that holt's method has less RMSE for both hardcover and paperback. 
##  from the plot we can that data cannot be approximated to a linear model. From the literature of holt's method we see that it uses 2 paramenters atleast  one for trend smooth and second for overall smoothing. since there is only one parameter we prefer to take simple expoential smoothing.


## c)
## from the above results we see that holt's method has less RMSE for both hardcover and paperback. 
##  from the plot we can that data cannot be approximated to a linear model. From the literature of holt's method we see that it uses 2 paramenters atleast  one for trend smooth and second for overall smoothing. since there is only one parameter we prefer to take simple expoential smoothing.
## RMSE, MAE, MPE is lower for holt's method. Among the series paperback and hardcover, hardcover had least RMSE MAE and MPE. From this inference we can conclude that hardcover timeseries did best among all.

## d)
# first forecast method for each series used was the holt's method

acc_hardcover_holt <- accuracy(results_hardcover_holt)
acc_hardcover_ses <- accuracy(results_hardcover_ses)
acc_paperback_holt <- accuracy(results_paperback_ses)
acc_paperback_ses <-accuracy(results_paperback_holt)

acc_hardcover_holt[,"RMSE"]

##Formula based - paperback method:holt
paperback_holt_upper <- results_paperback_holt$upper[1, "95%"]
paperback_holt_upper
paperback_holt_lower <- results_paperback_holt$lower[1, "95%"]
paperback_holt_lower 
##calculation
upper = results_paperback_holt$mean[1] + (1.96*(accuracy(results_paperback_holt)[,"RMSE"]))
upper 
lower = results_paperback_holt$mean[1] - (1.96*(accuracy(results_paperback_holt)[,"RMSE"]))
lower

#Fomula based - hardcover method: holt
hardcover_holt_upper <- results_hardcover_holt$upper[1, "95%"]
hardcover_holt_upper
hardcover_holt_lower <- results_hardcover_holt$lower[1, "95%"]
hardcover_holt_lower

# calculation
upper = results_hardcover_holt$mean[1] + (1.96*(accuracy(results_hardcover_holt)[,"RMSE"]))
upper 
lower = results_hardcover_holt$mean[1] - (1.96*(accuracy(results_hardcover_holt)[,"RMSE"]))
lower


# In holts method upper and lower limits are almost same to that of formula to calculated.


##Formula based - paperback method:ses
paperback_ses_upper <- results_paperback_ses$upper[1, "95%"]
paperback_ses_upper
paperback_ses_lower <- results_paperback_ses$lower[1, "95%"]
paperback_ses_lower 
##calculation
upper = results_paperback_ses$mean[1] + (1.96*(accuracy(results_paperback_ses)[,"RMSE"]))
upper 
lower = results_paperback_ses$mean[1] - (1.96*(accuracy(results_paperback_ses)[,"RMSE"]))
lower

#Fomula based - hardcover method: holt
hardcover_ses_upper <- results_hardcover_ses$upper[1, "95%"]
hardcover_ses_upper
hardcover_ses_lower <- results_hardcover_ses$lower[1, "95%"]
hardcover_ses_lower

# calculation
upper = results_hardcover_ses$mean[1] + (1.96*(accuracy(results_hardcover_ses)[,"RMSE"]))
upper 
lower = results_hardcover_ses$mean[1] - (1.96*(accuracy(results_hardcover_ses)[,"RMSE"]))
lower


# In ses method upper and lower limits are almost same to that of formula to calculated.

# The intervals produced by ses and holt are almost equal.


# 4)

## a)
summary(bricksq)

 bricksq%>%
  stl(t.window=4, s.window="periodic", robust=TRUE) %>%
  autoplot()
# Trend cycle  increases over time and becomes after a certain level (1980).
# seasonal component remains constant over time.
# remainder are low.
 
 
fix_chang_stl <-  bricksq%>%
   stl(t.window=4, s.window="periodic", robust=TRUE) %>%
   autoplot()

 #fixed seasonlity
fixed_sea_stl <- bricksq%>%
   stl(s.window="periodic", robust=TRUE)
 
 #changing seasonality
 
changing_sea_stl <- bricksq%>%
   stl(s.window=4, robust=TRUE)
 
   
#b)
autoplot(bricksq, series ="data") +autolayer(seasadj(fixed_sea_stl),series = "seasonally_adjusted_data")

autoplot(bricksq, series ="data") +autolayer(seasadj(changing_sea_stl),series = "seasonally_adjusted_data")
 
#autoplot(bricksq, series ="data") + autolayer(seasadj(fixed_sea_stl), series = "seasonally_adjusted_data")


#c)
 
 fit <- stl(bricksq, t.window=10, s.window="periodic",
            robust=TRUE)
 fit %>% seasadj() %>% naive() %>%
   autoplot() + ylab("New orders index") +
   ggtitle("Naive forecasts of seasonally adjusted data")
# The upper and lower limits of the prediction intervals on the seasonally adjusted data are “reseasonalised” by adding in the forecasts of the seasonal component.





