library(fpp2)
library(forecast)


#a)
#Reading the excel data
retaildata <- readxl::read_excel("/Users/chanukya/Downloads/retail.xlsx", skip =1)

#View(retaildata)

#b)
myts <- ts(retaildata[,"A3349873A"], frequency = 12, start = c(1982,4))


#c)
autoplot(myts)
#review: from the data we can see that it the data is  "Seasonal Cylical" untill almost 1998. From then till 2010 we see that there isn't much increasing trend

ggseasonplot(myts)
#ggseason plot is similar to autoplot except the data is plotted against the seasons in seperate years
# we can see that in the month of decemeber the sales are maximum.

ggsubseriesplot(myts)
# Each season is plotted in mini time series. 
# The line in the plot is the average in the plot. We can see that reatil average among the months of all the years is highest for decemeber month.

gglagplot(myts)
gglagplot(myts,seasonal = FALSE)
# gglagplot will plot time series aganist lagged versions of themselves. 

ggAcf(myts)
# if the data between the blue dotted line then the its considered to be white noise. from the plot we can that there is no white noise



#2)

autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)

#It can be considered season  if you take time interval of 10 years. Basically from the plot we can say it is cyclic.
#sales are maximum in the month of march and least in the month of december.
#the average sales for the entire timeperiod metioned is maximum for the month of March and least for the month of Dec
#we can see that there is some noise in the dataset.


autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths)
#The data is seasonal as we see for every 10 years there is a  decreasing trend.
#deaths are maximum in the month of july for all the years and minimum in the month of feb
#the average number of deaths are minimum for the month Feb month 
# the data is correlated, seasonal trend. 





autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq)
#Data lags, data is seasonal with upoward trend

autoplot(sunspotarea)
#ggseasonplot(sunspotarea)
#ggsubseriesplot(sunspotarea)
gglagplot(sunspotarea)
ggAcf(sunspotarea)
#Data is cylic.


#3)
mypigs <- window(pigs, start= 1990)
#View(mypigs)
autoplot(mypigs)+xlab("Year")+ylab("thousands")+ggtitle("Number of pigs slaughtered in victoria")
ggAcf(mypigs)
#data is cyclic and from the auto-corelation function we can see that the  most of the data is not correlated and it is mostly white noise.


#4
ddj <- diff(dj)
#length(dj)
autoplot(ddj)
ggAcf(ddj)
# data is least correlated. Almost entire data is white noise. 

