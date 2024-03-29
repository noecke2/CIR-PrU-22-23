
# Full Lasso Model -----------------------------------------------------




# Load in libraries and functions --------------------------------------------------------

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
source("lasso_models/multinomial_cv.R")



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
testing_data <- ag_dataset_model[-training_samples,]

# Sort out the different columns for social/natural/full
cols <- colnames(ag_dataset)
social <- cols[c(2:41, 47:50)]
natural <- cols[c(42:46)]
full <- c(social, natural)


# Define training data with just natural variables
full_train <- training_data[c(full, "is_satoyama2")]


# Table for displaying subset of the data for presentation ----------------



# full_train %>%
#   dplyr::select(natural[c(2:4)], social[c(35,37,43)], is_satoyama2) %>%
#   mutate(across(!c(avg_slope, is_satoyama2), round, 3)) %>%
#   rename(prop_6_fam = "prop_under6_families",
#          is_satoyama = "is_satoyama2",
#          prop_ag = "prop_ag_area",
#          prop_forest = "prop_forest_area") %>%
#   DT::datatable()
  


# Run lasso on natural training data --------------------------------------

# define x, the explanatory variables
x <- data.matrix(full_train[, !(colnames(full_train) == "is_satoyama2")])

# define y, the response variable (is_satoyama2)
y <- full_train$is_satoyama2

# Run lasso using cv.glmnet 
### This does cross validation for us (10 fold by default)


# cross validation and lasso
set.seed(2)
full_cv_fit_lasso <- cv.glmnet(x, y, alpha = 1, 
                               nfolds = 10, 
                               family = "multinomial",
                               type.multinomial = "grouped",
                               type.measure = "class")

plot(full_cv_fit_lasso,main = " ")
title(main = "Fig. 3: Lasso Regression with 10-fold Cross Validation - Full Model", line = 3, font.main = 1)

# Lambda values for full model
full_cv_fit_lasso$lambda.min
full_cv_fit_lasso$lambda.1se

combine_results(full_cv_fit_lasso, lambda = 0.0486)



#https://zitaoshen.rbind.io/project/machine_learning/how-to-plot-roc-curve-for-multiple-classes/

# Generate plot of coefficients ------------------------------------------------

# Choose a custom range for lambda: 1/100 to 1000 
lambdas <- 10^seq(-25, 10, length = 100)

# fit lasso with default lambdas
full_fit_lasso <- glmnet(x, y, alpha = 1, lambda = lambdas, family = "multinomial",
                         type.multinomial = "grouped",
                         type.measure = "class")

plot(full_fit_lasso, xvar = "lambda", label = TRUE)
title(main = "Figure 4: Lasso Regression with All Variables", line = 3, font.main = 1)





# Full table predictors and accuracy ---------------------------------------



acc_vec <- vector(mode = "numeric", length = 100)
preds_vec <- vector(mode = "numeric", length = 100)

lambdas <- 10^seq(-5, 0, length = 100)

acc_tbl <- tibble(lambda = lambdas)


idx <- 1
for (i in lambdas){
  mult_model <- build_multinomial(full_cv_fit_lasso, lambda = i)
  acc <- accuracy(mult_model)
  num_preds <- length(mult_model$coefnames) - 1
  preds_vec[idx] = num_preds
  acc_vec[idx] = acc
  idx <- idx + 1
}

build_multinomial(full_cv_fit_lasso, lambda = 0.01)

acc_tbl %>%
  mutate(acc = acc_vec,
         num_preds = preds_vec) %>% 
  print(n = 100)

build_multinomial(full_cv_fit_lasso, lambda = 0.0870)


build_multinomial(full_cv_fit_lasso, lambda = 0.0774)


cv_tbl <- tibble()
lambdas <- 10^seq(-5, 0, length = 50)


for (i in lambdas){
  
  mult_tbl <- build_multinomial_cv(full_cv_fit_lasso, lambda = i) %>%
                mutate(lambda = i) 
  cv_tbl <- bind_rows(cv_tbl, 
                      mult_tbl)  
}

cv_tbl %>%
  group_by(lambda, num_preds) %>%
  summarize(acc = mean(test_pred == test_actual)) %>% 
    print(n = 100)

build_multinomial(full_cv_fit_lasso, lambda = 0.0754)

model_lasso_full_9 <- multinom(is_satoyama2 ~ mtn_program_area + special_ag_mtn_villages + depopulated_area + num_orgs_mfngrants + pop_density + avg_altitude + prop_ag_area + prop_forest_area + prop_paddy_area, data = training_data)
predictions_lasso_full_9 <- predict(model_lasso_full_9, ag_dataset)
predictions_lasso_full_9_probs <- as.data.frame(predict(model_lasso_full_9, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_lasso_full_9_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_lasso_full_9_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                                 ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                        ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_lasso_full_9 <- predictions_lasso_full_9
ag_dataset$predictions_lasso_full_9_probmaxs <- predictions_lasso_full_9_probs$predictions_lasso_full_9_probmaxs
ag_dataset$predictions_lasso_full_9_num <- predictions_lasso_full_9_probs$predictions_lasso_full_9_num

# Adding each class's probability
ag_dataset$satoyama_prob <- predictions_lasso_full_9_probs$satoyama
ag_dataset$okuyama_prob <- predictions_lasso_full_9_probs$okuyama
ag_dataset$urban_prob <- predictions_lasso_full_9_probs$urban


# conflicts involve satoyama (no disagreement between okuyama and urban)
predictions_lasso_full_9_probs %>%
  # count(predictions_lasso_full_9_num) %>%
  filter(predictions_lasso_full_9_probmaxs < 0.75) %>%
  filter(satoyama > 0.05, urban > 0.05, okuyama > 0.05) 

# Write to CSV
ag_dataset %>% 
  # We need to add 'KEY' in front of the 10-digit shuraku key code, otherwise GIS will interpret it as an integer, not a string.
  # I took these out with the Calculate Field tool in order to go back to the straight-up 10-digit code (key = !key![3:])
  mutate(gis_key = paste0('KEY', key)) %>% 
  select(72:81) %>% 
  write_csv('model_preds/lasso_prob_layers.csv')


# Write only model preds to CSV
ag_dataset %>%
  mutate(gis_key = paste0('KEY', key)) %>%
  select(75:84) %>%
  write_csv('model_preds/all_model_preds.csv')







