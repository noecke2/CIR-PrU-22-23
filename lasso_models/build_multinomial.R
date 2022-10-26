
# Function to build the multinomial models  given a lasso output-------------------------------



library(nnet)
library(caret)
library(tidyverse)


# Will's accuracy function


accuracy <- function(x) {
  print(mean(x %>% predict(testing_data) == testing_data$is_satoyama2))
}

 

# Test code that was used to build the function ---------------------------

# Code to grab the variables kept by lasso
### Uses combine_results function defined in combine_results.R

# nat_coefs <- rownames(combine_results(natural_cv_fit_lasso, lambda = 0.3))[-1]
# 
# rownames(combine_results(natural_cv_fit_lasso, lambda = 0.3))[-1]
# 
# cat(paste(nat_coefs, collapse = "\n"))
# 
# formula <- paste(nat_coefs, collapse = ' + ')
# 
# accuracy <- function(x) {
#   print(mean(x %>% predict(testing_data) == testing_data$is_satoyama2))
# }


model <- multinom(as.formula(paste0("is_satoyama2", " ~ ", formula)), data = training_data)
model$fitted.values
accuracy(model)

# build_multinomial function - will build multinomial model using variables selected by lasso -------------------------------------------------------------------------


build_multinomial <- function(lasso_model, lambda) {
  
  coefs <- rownames(combine_results(lasso_model, lambda))[-1]
  
  cat("Variables selected during lasso: ", length(coefs), "\n", paste(coefs, collapse = "\n"), "\n\n")
  
  formula <- paste(coefs, collapse = " + ")
  
  mult_model <- multinom(as.formula(paste0("is_satoyama2", " ~ ", formula)), data = training_data)
  
  mult_model
}
