setwd("C:\\Users\\Admin\\OneDrive\\QMUL\\statistical_modelling")
getwd()

#Use R studio to ifnd hte best reduced model using the AIC procedure and state which 
# is the best reduced model 

data <- read.csv('Bridge.csv')
attach(data)
Y<- log(data[,2])
X1 <- log(data[,3])
X2 <- log(data[,4]) 
X3<- log(data[,5])
X4<- log(data[,6])
X5<- log(data[,7]) 

m1 <- lm(Y ~ X1 +X2+X3+X4+X5)
vif(m1)

reduced.model <- step(m1, direction = "backward")
modyn <- lm(Y~1) 
aic.forward.model <-step(modyn, scope =~X1 +X2+X3+X4+X5, direction ="forward")

#1b for the best reduced model , comment on the values of VIF 

modfinal <- lm(Y~ X3 +X5+X2) 
vif(modfinal) 


#1c 

modfinal <- lm(Y~X3 +X5+X2) 
summary(modfinal) 

#1d 

anova(modfinal)
shapiro.test(stdresfinal) 

#2 - why doesnt these matrix things match the data ?
model_full <- lm(Y~X1+X2) 

# Manually define XtX
XtX <- matrix(c(
  25,    219,    10232,
  219,   3055,   133899,
  10232, 133899, 6725688
), nrow = 3, byrow = TRUE)

# Manually define XtY
XtY <- matrix(c(
  559.60,
  7375.44,
  337071.69
), nrow = 3)

# Manually define XtX_inv
XtX_inv <- matrix(c(
  0.1132,   -0.0044,   -0.00008,
  -0.0044,    0.0027,   -0.00004,
  -0.00008,  -0.00004,   0.000001
), nrow = 3, byrow = TRUE)

# Manually define YtY
YtY <- 18310.63

# Manually define mean of Y
Y_bar <- 22.384

# (a) Full model
n <- 25
p <- 3
SST <- 5784.54
SSR <- 5550.81
SSE <- SST - SSR
sigma_sq_hat <- SSE / n

aic_a <- 2 * (p + 1) + n * (log(2 * pi) + log(sigma_sq_hat) + 1)

# VIF calculation using R^2 from SSR/SST
R2_a <- SSR / SST
vif_a <- 1 / (1 - R2_a)

# Output
cat("Model (a):\n")
cat("SSE =", SSE, "\n")
cat("σ̂² =", sigma_sq_hat, "\n")
cat("AIC =", aic_a, "\n")
cat("VIF =", vif_a, "\n")

# (b) Simpler model
SSR_b <- 5382.409
SSE_b <- 402.1338
sigma_sq_hat_b <- SSE_b / n
p_b <- 2

aic_b <- 2 * (p_b + 1) + n * (log(2 * pi) + log(sigma_sq_hat_b) + 1)

# VIF
R2_b <- SSR_b / (SSR_b + SSE_b)
vif_b <- 1 / (1 - R2_b)

# Output
cat("\nModel (b):\n")
cat("SSE =", SSE_b, "\n")
cat("σ̂² =", sigma_sq_hat_b, "\n")
cat("AIC =", aic_b, "\n")
cat("VIF =", vif_b, "\n")

#(a) AIC ≈ 134.83, VIF ≈ 24.75

#(b) AIC ≈ 146.39, VIF ≈ 14.38