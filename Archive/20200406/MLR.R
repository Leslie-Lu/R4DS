# Exercise 1:
cement <- data.frame(
  X1=c(7, 1, 11, 11, 7, 11, 3,1, 2, 21, 1, 11, 10),
  X2=c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
  X3=c( 6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
  X4=c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12),
  Y =c(78.5, 74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
)
# (1)
lm_full <- lm(Y ~ X1+X2+X3+X4, data = cement)
summary(lm_full)

# (2)
selectedMod <- step(lm_full)
selectedMod

# (3)
summary(selectedMod)

# (4)
drop1(lm_full, test = 'F')

# (5)
# the residual sum of squares for model lm_full:
sum(resid(lm_full)^2) 
# the residual sum of squares for model selectedMod:
sum(resid(selectedMod)^2)


# Exercise 2:
toothpaste <- data.frame(
  X1= c(-0.05, 0.25,0.60,0,0.25,0.20, 0.15,0.05,-0.15, 0.15,
    0.20, 0.10,0.40,0.45,0.35,0.30, 0.50,0.50, 0.40,-0.05, 
    -0.05,-0.10,0.20,0.10,0.50,0.60,-0.05,0,0.05, 0.55),
  X2= c( 5.50,6.75,7.25,5.50,7.00,6.50,6.75,5.25,5.25,6.00, 6.50,
    6.25,7.00,6.90,6.80,6.80,7.10,7.00,6.80,6.50, 6.25,6.00,6.50,
    7.00,6.80,6.80,6.50,5.75,5.80,6.80),
  Y = c(7.38,8.51,9.52,7.50,9.33,8.28,8.75,7.87,7.10,8.00, 
    7.89,8.15,9.10,8.86,8.90,8.87,9.26,9.00,8.75,7.95, 7.65,
    7.27,8.00,8.50,8.75,9.21,8.27,7.67,7.93,9.26) )
lm.sol <- lm(Y~X1+X2, data=toothpaste)
summary(lm.sol)
# 绘散点图和回归曲线:
attach(toothpaste)
plot(Y~X1)
abline(lm(Y~X1)) 

lm2.sol <- lm(Y~X2+I(X2^2)) 
x <- seq(min(X2), max(X2), len=200)
y <- predict(lm2.sol, data.frame(X2= x))
plot(Y~X2)
lines(x,y)

# confidence intervals for beta coefficients:
confint(lm.sol)
confint(lm2.sol)








