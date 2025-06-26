#1 
setwd("C:\\Users\\Admin\\OneDrive\\QMUL\\statistical_modelling")
getwd()

data <- read.csv('liver.csv')
x1 <- data$x1
x2<- data$x2
x3<- data$x3 
ly <- data$log10y
# extracts columns names x1,x2,x3 , log10y 

modly3 <- lm(ly ~x1 +x2+x3) 
summary(modly3)


stdres3 <- rstandard(modly3) 
fits3 <- fitted(modly3) 

#1b Assessing assumptions of normality and constant variance of random errors 
plot(fits3)
qqnorm(stdres3, main = "Q-Q plot, liver3")
qqline(stdres3)
# if stdres follows a normal distribution , points should lie on straight line
#here we can see points  dont lie on straight line 

#to further test normality assumption -> shapiro test 
shapiro.test(stdres3)
#pvalue = 0.001605 hence strong evidence against normality assumption 

#1c Compare model 3 with the two models in practical session. Which one is best and why 


#1d for best model find leverage values and cook's distance 
hat <- hatvalues(modly3) #computes leverage/ hat vlaues 
cook <- cooks.distance(modly3) 
i<- 1:54 # creates sequence of 54 integers for 54 observations 
plot(i, hat, main = "Leverage values")
plot(i, cook, main = "Cooks distance values")
qf(0.5, 3, 51) 

# points with high leverage may be outliers 
# points with high cook's distance mightly really affect regression model / need to remove 




#QUESTION2 
setwd("C:\\Users\\Admin\\OneDrive\\QMUL\\statistical_modelling")
getwd()

data2<- read.csv("marketdata.csv")
Y <- data2$Mkt_4
X1 <- data2$Mkt_1
X2 <- data2$Mkt_2
X3 <- data2$Mkt_3

#2a 

model1 <-lm(Y~X1)
summary(model1)
stdres1 <- rstandard(model1)
# X1-Max = 1.12e-07 (p-value/ chance this result is random) = very small hence X1 is statistically significant 
#                           meansing X1 has a strong and meaningful impact on Y 
# intercept - Max = 0.952 = very large hence intercept is not statistically significant 
# B1/ X1 = 0.8733 (gradient is strongly positive) 
# intercept = 0.0001395 = very small and not important (from intercept-Max)
#R^2 value 0.9463 = very large so model explains most of variation so good fit 

model2<- lm(Y ~X2) 
summary(model2)
stdres2 <- rstandard(model2) 
# X2-Max = 0.0131 (p-value/ chance this result is random) = (<0.05) small 
#             hence X1 is statistically significant meansing X1 has a strong and meaningful impact on Y( )only at 0.05 level)

# intercept - Max = 0.953 = very large hence intercept is not statistically significant 
# B1/ X1 = 0.44 (gradient is strongly positive) 
# intercept = 0.0004339 = very small and not important (from intercept-Max)
#R^2 = 0.4752 (<0.3) not very large hence not really a good fit 

model3<- lm(Y~X1+X2) 
summary(model3) 
stdres <- rstandard(model3)
# X1-Max = 5.79e-06 (p-value/ chance this result is random) = (<0.05) small 
#            hence X1 is statistically significant meansing X1 has a strong and meaningful impact on Y( )only at 0.05 level)
# X2-Max = large (>0.05)  =  not statiscally significant 
# intercept - Max = 0.979 = very large hence intercept is not statistically significant 

# intercept = 6.311e-05 = very small and not important (from intercept-Max)
#R^2 = 0.9518 and adjusted : 0.9411  very large hence  a good fit (but doesnt beat model1)


#2b 

shapiro.test(stdres1) 
shapiro.test(stdres2) 
shapiro.test(stdres3) 

#in all these models shapiro wilk does not reject the null hypothesis 
#thus we are not rejecting the normality assumption in all models 

#2c 
# best model is model 1 due to principle of parsimony 
# looking at R^2 values between model 1 and 3 but choose model 1 as model 3 does not have statistically significant parameters 


#2d for the best model , compute predicted values when new data are x1 vector of values
# For example, create a sequence of X1 values ranging from the minimum to maximum of the original X1 values
new_X1_values <- seq(min(X1), max(X1), length.out = 12)
predictions <- predict(model1, newdata = data.frame(X1 = new_X1_values))



#2e 
# Compute prediction intervals at 95% and 90% confidence levels
pred_interval_95 <- predict(model1, newdata = data.frame(X1 = new_X1_values), interval = "prediction", level = 0.95)
pred_interval_90 <- predict(model1, newdata = data.frame(X1 = new_X1_values), interval = "prediction", level = 0.90)

# Extract predicted values and intervals for plotting
predicted_values <- pred_interval_95[, 1]
lower_95 <- pred_interval_95[, 2]
upper_95 <- pred_interval_95[, 3]

lower_90 <- pred_interval_90[, 2]
upper_90 <- pred_interval_90[, 3]

# Plot the predicted values with prediction intervals
plot(new_X1_values, predicted_values, type = "l", col = "blue", lwd = 2, xlab = "X1", ylab = "Predicted Y", main = "Predicted Values and Prediction Intervals")
lines(new_X1_values, lower_95, col = "red", lty = 2)
lines(new_X1_values, upper_95, col = "red", lty = 2)
lines(new_X1_values, lower_90, col = "green", lty = 2)
lines(new_X1_values, upper_90, col = "green", lty = 2)
legend("topright", legend = c("Predicted", "95% Interval", "90% Interval"), col = c("blue", "red", "green"), lty = 1:2)


#2f 
actual_values <- Y

# Compute residuals for the 95% and 90% intervals
residuals_95 <- actual_values - predicted_values
residuals_90 <- actual_values - predicted_values

# Check if actual values fall within the 95% and 90% prediction intervals
within_95 <- actual_values >= lower_95 & actual_values <= upper_95
within_90 <- actual_values >= lower_90 & actual_values <= upper_90

# Print out the proportion of points within the prediction intervals
cat("Proportion within 95% interval:", mean(within_95), "\n")
cat("Proportion within 90% interval:", mean(within_90), "\n")

# Plot residuals to visualize how far the predicted values are from actual values
plot(new_X1_values, residuals_95, col = "red", pch = 16, xlab = "X1", ylab = "Residuals", main = "Residuals for 95% Prediction Interval")
abline(h = 0, col = "black", lty = 2)

plot(new_X1_values, residuals_90, col = "green", pch = 16, xlab = "X1", ylab = "Residuals", main = "Residuals for 90% Prediction Interval")
abline(h = 0, col = "black", lty = 2)

