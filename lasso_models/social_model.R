
# Social Lasso Model -----------------------------------------------------




# Load in libraries --------------------------------------------------------

library(Matrix)
library(glmnet)
library(tidyverse)
library(rlang)
library(gt)
library(caret)
library(nnet)
library(broom)
source("lasso_models/combine_results.R")


# Load in Will's Data and make Training/Testing ---------------------------

# Load in Will's Final Data
final_data <- read.csv("final_dataset.csv")

# Load in Will's Training/Testing Data
ag_dataset <- read_csv('final_dataset.csv')

ag_dataset_model <- ag_dataset %>% 
  filter(is_satoyama2 %in% c('urban', 'okuyama', 'satoyama')) %>% 
  mutate(is_satoyama2 = fct_relevel(is_satoyama2, 'satoyama'))

set.seed(1)
training_samples <- ag_dataset_model$is_satoyama2 %>% 
  createDataPartition(p = 0.8, list = FALSE)
training_data  <- ag_dataset_model[training_samples, ]
testing_data <- ag_dataset_model[-training_samples]

# Sort out the different columns for social/natural/full
cols <- colnames(ag_dataset)
social <- cols[c(2:41, 47:50)]
natural <- cols[c(42:46)]
full <- c(social, natural)

# Run lasso on natural training data --------------------------------------


# Define training data with just social variables
social_train <- training_data[c(social, "is_satoyama2")]

# define x, the explanatory variables
x <- data.matrix(social_train[, !(colnames(social_train) == "is_satoyama2")])

# define y, the response variable (is_satoyama2)
social_train2 <- social_train %>%
  mutate(is_satoyama3 = ifelse(is_satoyama2 == "urban", 0, ifelse(is_satoyama2 == "okuyama", 1, 2)))



y <- social_train2$is_satoyama3

# Run lasso using cv.glmnet 
### This does cross validation for us (10 fold by default)


# cross validation and lasso
set.seed(2)
social_cv_fit_lasso <- cv.glmnet(x, y, alpha = 1, 
                                 nfolds = 10, 
                                 family = "multinomial")

plot(social_cv_fit_lasso,main = " ")
title(main = "Lasso Regression with 5-fold Cross Validation - Social Model", line = 3, font.main = 1)

# Lambda values for the model
social_cv_fit_lasso$lambda.min
social_cv_fit_lasso$lambda.1se

### Display coefficients for all 3 layers
combine_results(social_cv_fit_lasso)


# Generate plot of coefficients ------------------------------------------------

# Choose a custom range for lambda: 1/100 to 1000 
lambdas <- 10^seq(-25, 10, length = 100)

# fit lasso with default lambdas
social_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lambdas, family = "multinomial")

plot(social_fit_lasso, xvar = "lambda", label = TRUE)
title(main = "Figure 4: Lasso Regression", line = 3, font.main = 1)
