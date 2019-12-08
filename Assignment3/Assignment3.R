#1)
library(fpp2)
View(fancy)
autoplot(fancy)
autoplot(log(fancy))
#fancy1 <- ts(fancy, frequency = 12, start = c(1988,4))
#View(fancy1)
#autoplot(fancy1)
#sales are increasing from january to december and sales are highest for month december.

#a) 
autoplot(log(fancy))

#b)
#variation is different across the plot so inorder make them almost equal so that it would fit well into the model.
surfing_festival <- c()
fancy1 <- time(fancy)
for (i in 1:length(fancy1)){
  if ((round(12*(fancy1[i] - floor(fancy1[i]))) + 1) ==3){
      surfing_festival[i] <- 1
    }
  else{
    surfing_festival[i] <- 0
  }
}
print(surfing_festival)

#c)
fancy_lm <- tslm(log(fancy)~trend+season+surfing_festival)
autoplot(fancy_lm$residuals)
acf(fancy_lm$residuals)
# we see that there is correlation intially but with increase in time the correlation is decreasing.


#d)
#Breusch-Godfrey test is used checking the autocorrelation in residuals

#e)
fancy_dependent <- c()
for(i in 1:36){
  if(i %% 12 == 3){
    fancy_dependent[i] <- 1
  }
  else{
    fancy_dependent[i] <- 0
  }
}
fancy_dependent <- ts(data = fancy_dependent,start= 1994, end=c(1996,12), frequency = 12)
predicted_fancy <- forecast(fancy_lm, newdata = data.frame(Time = time(fancy_dependent), surfing_festival = fancy_dependent))
autoplot(predicted_fancy)

#f)
Intervals <- c(predicted_fancy$lower, predicted_fancy$upper)
lower = Intervals[1]
lower
upper = Intervals[length(Intervals)]
upper

#2) 
gasoline_2004 <- window(gasoline, end = 2005)
#View(gasoline_2004)
# a)
gasolin_2004_1 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,1))
gasolin_2004_3 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,3))
gasolin_2004_5 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,5))
gasolin_2004_7 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,7))
gasolin_2004_20 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,20))
autoplot(gasoline_2004) +autolayer(gasolin_2004_1$residuals)+xlab("fitted values")+ylab("gasoline_values_1")
autoplot(gasoline_2004) +autolayer(gasolin_2004_3$residuals)+xlab("fitted values")+ylab("gasoline_values_3")
autoplot(gasoline_2004) +autolayer(gasolin_2004_5$residuals)+xlab("fitted values")+ylab("gasoline_values_5")
autoplot(gasoline_2004) +autolayer(gasolin_2004_7$residuals)+xlab("fitted values")+ylab("gasoline_values_7")
autoplot(gasoline_2004) +autolayer(gasolin_2004_20$residuals)+xlab("fitted values")+ylab("gasoline_values_20")

#fitted values donot follow any trend where as data is getting aligned in a  line as value of k  increases

# b)
#aicc = 0
k = 0
m = 0
bic = 0
l = c()
m = c()
for (i in 1:20){
  number = i
  sample <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,number)) %>%CV()
  #print(sample["AIC"])
  
  l <- c(l,sample["AICc"])
  m <- c(m,sample["CV"])
}
#i = 6
#num = i
#sample <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,num)) %>%CV()
#sample["AIC"]
#print(aicc)
#print(bic)
min_l <- min(l)
min_m <- min(m)
min_l
min_m
match(min_l,l)
match(min_m,m)


# c)
gasolin_2004_5 <- tslm(gasoline_2004 ~ trend + fourier(gasoline_2004,12))
checkresiduals(gasolin_2004_5)

#d)
gasoline_2005 <- forecast(gasolin_2004_5,newdata=data.frame(fourier(gasoline_2004, K = 12, h = 52)))
#for next year this are the preidcted values


## 3)




