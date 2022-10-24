
# Creat function to combine lasso coefficients of multinomial model -------


combine_results <- function(model, lambda = model$lambda.1se) {
  temp <- coef(model, s = lambda)
  beta <- Reduce(cbind, temp)
  beta <- beta[apply(beta != 0, 1, any),]
  colnames(beta) <- names(temp)
  beta
}
