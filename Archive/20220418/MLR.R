rm(list = ls())
gc()

# install.packages(c('tidyverse', 'data.table'))
library(tidyverse)
# remove.packages("data.table") #/Library/Frameworks/R.framework/Versions/4.1/Resources/library
# install.packages("data.table", type = "source",
#                  repos = "https://Rdatatable.gitlab.io/data.table")
# library(data.table)
library(magrittr)
# install.packages('psych')
library(psych)
library(MASS)

data= read.csv('Archive/20220418/redwine.csv') %>% as_tibble()


# 1. Report descriptive statistics of the data and see if there is any evidence of missing or miscoded values. 
# If any, address them properly (if none, just move on).
# Anwser:
data %>% describe()
data %>% str()
# From the output above, there is no evidence of miscoded values.
identical(data, data %>% na.omit())
# From the output above, there is no evidence of missing values.

# 2. Get a correlation matrix of all variables in the data. Report correlations to two decimal places.
# Anwser:
corr_mat <- cor(data)
round(corr_mat, 2)

# 3. Create a set of scatter plots where the vertical axis is quality and the horizontal axis is each predictor.
# Anwser:
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,4))
for (i in 1:11) {
  plot(data[[i]], data$quality, xlab= names(data)[i], ylab="quality")
}
par(opar)

# 4. Estimate a regression of quality on one predictor and report the results. 
# The predictor here is the variable with the highest correlation with quality.
# Anwser:
# The variable with the highest correlation with quality is alcohol, and the correlation coefficient is 0.48.
SLR= data %>% select(quality, alcohol) %>% 
  lm(quality ~ alcohol, data = .)
SLR %>% summary()
# From the output above:
# - the estimated regression line equation can be written as follow: quality= 0.36084 * alcohol + 1.87497.
# - the intercept (b0) is 1.87497. 
# It can be interpreted as the predicted quality score unit for a zero percentage of alcohol. 
# This means that, for a percentage of alcohol equal zero, we can expect a quality score of 1.87497.
# - the regression beta coefficient for the variable alcohol (b1), also known as the slope, is 0.36084. 
# This means that, for a percentage of alcohol equal to one percent, we can expect an increase of 0.36084 units in quality,
# that is, quality = 0.36084 * 1 + 1.87497 = 2.23581. 

# 5. Estimate a regression of quality on three predictors and report the results. 
# The predictors here include three variables with the highest correlation with quality.
# Anwser:
# The three variables with the highest correlation with quality is alcohol, volatile.acidity and sulphates, 
# and the correlation coefficients are 0.48, -0.39, 0.25, respectively.
MLR= data %>% dplyr::select(quality, alcohol, volatile.acidity, sulphates) %>% 
  lm(quality ~ ., data = .)
MLR %>% summary()
# It can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. 
# This means that, at least, one of the predictor variables is significantly related to the outcome variable quality.
# It can be seen that, changing in all three predictors are significantly associated to changes in quality. 
# For example, for a fixed amount of alcohol and volatile.acidity, 
# increasing an additional one on sulphates leads to an increase in quality by approximately 0.67903*1 = 0.67903 quality score units, on average.
# The model equation can be written as follow: 
# quality = 2.61083 + 0.30922*alcohol - 1.22140*volatile.acidity + 0.67903*sulphates.

# 6. Estimate a regression of quality on all other variables and report the results.
# Anwser:
MLR_full= data %>% 
  lm(quality ~ ., data = .)
MLR_full %>% summary()
# It can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. 
# This means that, at least, one of the predictor variables is significantly related to the outcome variable quality.
# It can be seen that, changing in volatile.acidity, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates and alcohol are significantly associated to changes in quality,
# while changes in the rest variables are not significantly associated with quality.
# The model equation can be written as follow: 
# quality = 2.61083 + 0.30922*alcohol - 1.22140*volatile.acidity + 0.67903*sulphates.

# 7. Conduct a 4-fold cross validation on the models you’ve estimated in Questions 4, 5, and 6 and report their CVE values. 
# If all done correctly, the CVE should be the highest for the one estimated in Question 4, and the lowest for Question 6.
# Anwser:
cv_4= function(data, k, lm_formula){
  set.seed(1)
  dataf <- data[sample(nrow(data), replace=FALSE),]
  folds <- cut(seq(1,nrow(dataf)), breaks=k, labels=FALSE)
  mspe <- rep(NA, k)
  for(i in 1:k){
    train <- dataf[folds!=i,] 
    test <- dataf[folds==i,] 
    newlm <- lm(lm_formula %>% as.formula(), data = train)
    test$newpred <- predict(newlm, newdata=test)
    mspe[i] <- sum((test$quality - test$newpred)^2) / nrow(test)
  }
  cve <- mean(mspe)
  return(cve) 
}
SLR_CVE= cv_4(data = data, k= 4, lm_formula= 'quality ~ alcohol')
SLR_CVE
MLR_CVE= cv_4(data = data, k= 4, lm_formula= 'quality ~ alcohol + volatile.acidity + sulphates')
MLR_CVE
MLR_full_CVE= cv_4(data = data, k= 4, lm_formula= 'quality ~ .')
MLR_full_CVE
# The CVEs of the models estimated in Questions 4, 5, and 6 are 0.5047332, 0.4340933, 0.4226873.
# So, the CVE is the highest for the one estimated in Question 4, and the lowest for Question 6.

# 8. Find one alternative model fit that yields lower CVE than the ones you’ve estimated in Question 7. 
# Present it’s regression results and CVE (hint: consider feature selection, polynomial regression, or taking log on some of the predictors).
# Anwser:
# Revised model (incorporating log-transformed variables and poly structure):
Revised_model= lm(quality ~ log(fixed.acidity + 0.01) + volatile.acidity + 
                  poly(citric.acid, 3, raw = T) + log(residual.sugar + 0.01) +
                  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + log(density + 0.01) +
                  pH + sulphates + alcohol, data = data)
Revised_model %>% summary()
# Feature selection (drop citric.acid)
fs1= lm(quality ~ log(fixed.acidity + 0.01) + volatile.acidity + 
        log(residual.sugar + 0.01) +
        chlorides + free.sulfur.dioxide + total.sulfur.dioxide + log(density + 0.01) +
        pH + sulphates + alcohol, data = data)
fs1 %>% summary()
# Feature selection (drop pH)
fs2= lm(quality ~ log(fixed.acidity + 0.01) + volatile.acidity + 
        log(residual.sugar + 0.01) +
        chlorides + free.sulfur.dioxide + total.sulfur.dioxide + log(density + 0.01) +
        sulphates + alcohol, data = data)
# The model fs2's regression result is as following:
fs2 %>% summary()
fs2_CVE= cv_4(data = data, k= 4, 
              lm_formula= 'quality ~ log(fixed.acidity + 0.01) + volatile.acidity + 
              log(residual.sugar + 0.01) +
              chlorides + free.sulfur.dioxide + total.sulfur.dioxide + log(density + 0.01) +
              sulphates + alcohol')
fs2_CVE
# And the model fs2 yields lower CVE than the ones estimated in Question 7, which is 0.4214754.

# 9. Interpret the coefficient estimate on alcohol (i.e., beta estimate associated with alcohol variable).
# Anwser:
fs2 %>% summary()
# It can be seen that, changing in alcohol is significantly associated to changes in quality. 
# The regression beta coefficient for the variable alcohol is 2.355e-01. 
# This means that, for a percentage of alcohol equal to one percent, we can expect an increase of 2.355e-01 units in quality.

# 10. 
# Anwser:
new_obs <- tibble(
  fixed.acidity = 8.35,
  volatile.acidity= .5,
  citric.acid= .25,
  residual.sugar= 2.6,
  chlorides= .08,
  free.sulfur.dioxide= 15.5,
  total.sulfur.dioxide= 45,
  density= 1, 
  pH= 3,
  sulphates= .7,
  alcohol= 10.6
)
predicted_quality= predict(fs2, newdata= new_obs)
predicted_quality
# The predicted quality score of the new wine is 5.610259, which is less than 6.5.
# So, it will be considered "favorable" response.

# 11. How much do you trust this prediction from the estimated model? Discuss. 
# Anwser:
# I have used log transformation and poly structure on some variables, conducted feature selection,
# and perform cross validation on the estimated model from Question 8.
# And that model gives the lowest ross validation error.
# So, it's reliable to produce that prediction.

# 12. Suppose you presented this finding to the managerial staffs, 
# and they ask you to provide suggestions on how to improve consumer response. 
# Based on your analyses so far, what would you suggest? 
# Anwser:
# I would suggest that, to get more favourable responses from consumers,
# they should focus on the varables like fixed.acidity, volatile.acidity, residual.sugar,
# chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, sulphates and alcohol.
# Because these attributes obviously influence the quality score of wine significantly based on my analyses.

# 13. Think about the data carefully. I feel that there is one critical problem that makes
# the actual consumer response very different from what we see in the analyses. What is the problem? Discuss.
# Anwser:
# Our regression may overfit the existing data, which sometimes leads to lower generalization in different scenarioes.








