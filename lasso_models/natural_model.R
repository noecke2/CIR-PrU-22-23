
# Natural Lasso Model -----------------------------------------------------




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
source("lasso_models/build_multinomial.R")



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
testing_data <- ag_dataset_model[-training_samples, ]

# Sort out the different columns for social/natural/full
cols <- colnames(ag_dataset)
social <- cols[c(2:41, 47:50)]
natural <- cols[c(42:46)]
full <- c(social, natural)


# Define training data with just natural variables
natural_train <- training_data[c(natural, "is_satoyama2")]


# Run lasso on natural training data --------------------------------------

# define x, the explanatory variables
x <- data.matrix(natural_train[, !(colnames(natural_train) == "is_satoyama2")])

# define y, the response variable (is_satoyama2)
y <- natural_train$is_satoyama2

# Run lasso using cv.glmnet 
### This does cross validation for us (10 fold by default)


# cross validation and lasso
set.seed(2)
natural_cv_fit_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10, 
                                  family = "multinomial",
                                  type.measure = "class",
                                  type.multinomial = "grouped")

plot(natural_cv_fit_lasso,main = " ")
title(main = "Fig. 3: Lasso Regression with 10-fold Cross Validation - Natural Model", line = 3, font.main = 1)

natural_cv_fit_lasso$lambda.min
natural_cv_fit_lasso$lambda.1se

coef(natural_cv_fit_lasso, s = 0.25)

combine_results(natural_cv_fit_lasso, lambda = 0.45)
build_multinomial(natural_cv_fit_lasso, lambda = natural_cv_fit_lasso$lambda.1se)


# Generate plot of coefficients ------------------------------------------------

# Choose a custom range for lambda: 1/100 to 1000 
lambdas <- 10^seq(-25, 10, length = 100)

# fit lasso with default lambdas
natural_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lambdas, family = "multinomial", type.multinomial = "grouped")

plot(natural_fit_lasso, xvar = "lambda", label = TRUE)
title(main = "Figure 4: Lasso Regression", line = 3, font.main = 1)

coef(natural_fit_lasso, s = natural_fit_lasso$lambda[31])



# Building multinomial models ---------------------------------------------


# All 5 vars
accuracy(build_multinomial(natural_cv_fit_lasso, lambda = natural_cv_fit_lasso$lambda.1se))

# 4 vars - surprisingly accuracy is actually higher
accuracy(build_multinomial(natural_cv_fit_lasso, lambda = 0.1))

# 3 vars
accuracy(build_multinomial(natural_cv_fit_lasso, lambda = 0.2))

# 2 vars
accuracy(build_multinomial(natural_cv_fit_lasso, lambda = 0.42)) # 2 vars


# Code to build table with number of predictors and multinomial ac --------


acc_vec <- vector(mode = "numeric", length = 100)
preds_vec <- vector(mode = "numeric", length = 100)

lambdas <- 10^seq(-5, 0, length = 100)

acc_tbl <- tibble(lambda = lambdas)


idx <- 1

for (i in lambdas){
  mult_model <- build_multinomial(natural_cv_fit_lasso, lambda = i)
  acc <- accuracy(mult_model)
  num_preds <- length(mult_model$coefnames) - 1
  preds_vec[idx] = num_preds
  acc_vec[idx] = acc
  idx <- idx + 1
}

acc_tbl %>%
  mutate(acc = acc_vec,
         num_preds = preds_vec) %>% 
  print(n = 100)


<<<<<<< HEAD
build_multinomial(natural_cv_fit_lasso, lambda = 0.00001)


=======


# Using cross validation multinomial approach  ----------------------------


# Using less lambdas due to longer computational time

natural_cv_tbl <- tibble()
lambdas <- 10^seq(-5, 0, length = 50)


for (i in lambdas){
  
  mult_tbl <- build_multinomial_cv(natural_cv_fit_lasso, lambda = i) %>%
    mutate(lambda = i) 
  natural_cv_tbl <- bind_rows(natural_cv_tbl, 
                             mult_tbl)  
}

natural_cv_tbl %>%
  group_by(lambda, num_preds) %>%
  summarize(acc = mean(test_pred == test_actual)) %>% 
  print(n = 100)
>>>>>>> c124f2f94027e885e8835381e801708f0e24ab75
