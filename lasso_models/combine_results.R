
# Creat function to combine lasso coefficients of multinomial model -------


combine_results <- function(model) {
  temp <- coef(model, s = model$lambda.1se)
  beta <- Reduce(cbind, temp)
  beta <- beta[apply(beta != 0, 1, any),]
  colnames(beta) <- names(temp)
  beta
}
