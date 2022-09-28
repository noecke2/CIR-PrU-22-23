library(Matrix)
library(glmnet)


# Load in Will's Final Data
final_data <- read.csv("final_dataset.csv")

# Are there NA values present in the data?
is.na(final_data)
colSums(is.na(final_data))
# there do not appear to be any NA values - this is good for lasso regression

# issue with creating a numeric vector (levels are categorical - need to be numeric)
# tried mutate and ifelse commands, but not sure how to designate multi-leveled categorical variables with numeric indicators (1, 2, 3, etc.)
table(final_data$is_satoyama2)
# training data is set up the same way but for some reason it works??
table(training_data$is_satoyama2)
colSums(is.na(training_data))
dim(training_data)
head(training_data)
summary(training_data)

training_data2 <- training_data %>%
  select(-52, -53, -54)
         
summary(X)

# LASSO FOR NATURAL PREDICTORS
# Trim off first column (intercept), leaving only the predictors
X <- data.matrix(training_data2 %>% select(-is_satoyama2))

# Create numeric vector of responses
y <- as.numeric(training_data2$is_satoyama2) 

# trying example code for lasso regression
cv_model <- cv.glmnet(X, y, alpha = 1, family = "multinomial")
plot(cv_model)

best_model <- glmnet(X, y, alpha = 1, lambda = 0.1)
coef(best_model)

predict(best_model, s = 0.1, newx = training_data2 %>% select(-is_satoyama2))

predict(best_model, s = 0.1, newx = as.matrix(testing_data %>% select(-52, -53, -54, -59)))

# Choose a custom range for lambda: 1/100 to 1000 
lambdas <- 10^seq(-25, 10, length = 100)

# fit lasso with default lambdas
fit.lasso <- glmnet(X, y, alpha = 1, lambda = lambdas, family = "multinomial")

plot(fit.lasso, xvar = "lambda", label = TRUE)
title(main = "Figure 4: Lasso Regression", line = 3, font.main = 1)

# cross validation and lasso
fit.lasso.cv <- cv.glmnet(X, y, alpha = 1, nfolds = 5, family = "multinomial")

plot(fit.lasso.cv,main = " ")
title(main = "Fig. 3: Lasso Regression with 5-fold Cross Validation", line = 3, font.main = 1)

fit.lasso.cv$lambda.min
fit.lasso.cv$lambda.1se

# coefficients for the two lambda choices
coef(fit.lasso.cv, s = "lambda.min")
coef(fit.lasso.cv, s = "lambda.1se")
# coef(fit.lasso.cv, s = lambdas[])
