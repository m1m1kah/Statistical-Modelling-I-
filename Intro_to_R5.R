#1 

x<-c(3.4,1.8,4.6,2.3,3.1,5.5,0.7,3.0,2.6,4.3,2.1,1.1,6.1,4.8,
        3.8)
y<-c(26.2,17.8,31.3,23.1,27.5,36.0,14.1,22.3,19.6,31.3,24.0,
     17.3,43.2,36.4,26.1)
plot(x,y, main = "plot of Y versus X")
fire<-lm(y~x)
abline(fire)
summary(fire) 

#2
#this generates 100 evenly spaces values between 
#the min and max of x , useful for smooth plotting 
newx <- seq(min(x), max(x), length.out =100)
print(newx)

#3
#computes a 90& confidence interval for predictions
preds <-predict(fire, newdata = data.frame(x=newx), interval = 'confidence')
head(preds)

preds90 <- predict(fire, newdata = data.frame(x=newx), interval = 'confidence', level = 0.90)
head(pred90)

#4
#gives predictio intervals which are wider than 
#confidence intervals since they account for future variability
preds1<- predict(fire, newdata = data.frame(x=newx), interval = 'prediction')
head(preds1)

preds190 <-predict(fire, newdata = data.frame(x=newx), interval = 'prediction', level = 0.90)
head(preds190)

#5 to do with above 

#6 
plot(x,y,main = 'Plot of Y versus X')
abline(fire) 
lines(newx, preds[ ,3], lty = 'dashed', col='blue') #plots upper CI 
lines(newx, preds[ ,2], lty = 'dashed', col='blue')#plots lower CI 
lines(newx, preds[ ,3], lty = 'dashed', col='red')#plots the upper PI 
lines(newx, preds[ ,2], lty = 'dashed', col='red')#plots the lower PI 



