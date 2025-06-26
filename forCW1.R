x<-c(71, 64, 43, 67, 56, 73, 68, 56, 76, 65, 45, 58, 45, 53, 49, 78)
y<-c(82,  91, 100, 68, 87, 73, 78, 80, 65, 84, 116, 76, 97, 100, 105, 77)

muscle <- lm(y~x)
summary(muscle)
anova(muscle)

prediction <- predict(muscle, newdata = data.frame(x=65))
prediction

stdres = rstandard(muscle) 
qqnorm(stdres) 
qqline(stdres) 
shapiro.test(stdres)

df <- length(x) - 2 
t_alpha2 <- qt(1 - 0.005,df )

lower_bound <- -1.0236 - t_alpha2 * 0.1882
upper_bound <- -1.0236 + t_alpha2 * 0.1882
