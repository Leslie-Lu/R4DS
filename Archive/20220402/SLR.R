library(tidyverse)
library(magrittr)
data= read.csv('ecommerce_customers.csv') %>% as_tibble()

# 1. Estimate a simple linear regression of y on x and report the R results.
# Anwser:
model= data %>% select(Yearly.Amount.Spent, Time.on.App) %>% 
  lm(Yearly.Amount.Spent ~ Time.on.App, data = .)
model %>% summary()
# From the output above:
# - the estimated regression line equation can be written as follow: Yearly.Amount.Spent= 39.834 * Time.on.App + 19.209
# - the intercept (b0) is 19.209. 
# It can be interpreted as the predicted Yearly.Amount.Spent unit for a zero Time.on.App. 
# This means that, for a Time.on.App equal zero, we can expect a Yearly.Amount.Spent of 19.209 dollars.
# - the regression beta coefficient for the variable Time.on.App (b1), also known as the slope, is 39.834. 
# This means that, for a Time.on.App equal to one minute, we can expect an increase of 39.834 units in Yearly.Amount.Spent,
# that is, Yearly.Amount.Spent = 39.834 * 1 + 19.209 = 59.043 dollars. 

# 2. Write down the estimated regression model.
# Anwser:
Yearly.Amount.Spent= 39.834 * Time.on.App + 19.209

# 3. Create a scatter plot of Yearly.Amount.Spent (y) vs. Time.on.App (x). 
# Superimpose the estimated regression line on the scatter plot.
# Anwser:
ggplot(data, aes(Time.on.App, Yearly.Amount.Spent)) + 
  geom_point() +
  stat_smooth(method = lm)

# 4. Interpret R2 of the estimated regression model.
# Anwser:
# The R2 measures, how well the model fits the data. 
# In this question, R2 equals 0.2493, 
# which means the estimated regression model explains about 24.93% of the variation in Yearly.Amount.Spent.

# 5. Conduct hypothesis testing for the association between Time.on.App and Yearly.Amount.Spent.
# State the null hypothesis and interpret the test results.
# Anwser:
# Hypothesis testing: testing the significance of regression beta coefficient
# Null hypothesis (H0): the coefficients are equal to zero (i.e., no association between Time.on.App and Yearly.Amount.Spent)
# Alternative Hypothesis (Ha): the coefficients are not equal to zero (i.e., there is a significant association between Time.on.App and Yearly.Amount.Spent)
# Test statistic equals 12.861, and the p value is less than 0.05,
# so we can reject the null hypothesis and accept the alternative hypothesis, 
# which means that there is a significant association between Time.on.App and Yearly.Amount.Spent.

# 6. Calculate the expected annual spending for a customer who spends 10 minutes on phone application.
# Anwser:
39.834 * 10 + 19.209
# the expected annual spending for a customer who spends 10 minutes on phone application is 417.549 

# 7. Calculate how much additional annual spending will be made by a customer who
# spends about 3 more minutes than others on the phone application.
# Anwser:
39.834 * 3
# 119.502$ of additional annual spending will be made by a customer who spends about 3 more minutes than others on the phone application

# 8. Calculate residual sum of squares of the estimated regression model.
# Anwser:
data$Yearly.Amount.Spent_hat <- model$coefficients[1] + (model$coefficients[2] * data$Time.on.App) 
(rss= sum((data$Yearly.Amount.Spent - data$Yearly.Amount.Spent_hat)^2))

# 9. Demonstrate that an alternative model fit, yˆ = 20 + 38 · x, has a higher residual sum of squares than the estimated regression model.
# Anwser:
data$Yearly.Amount.Spent_hat <- 20 + (38 * data$Time.on.App) 
(rss= sum((data$Yearly.Amount.Spent - data$Yearly.Amount.Spent_hat)^2))




