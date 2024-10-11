# generate training set
X1 <- rnorm(1000, 0, 10)
X2 <- rnorm(1000, 0, 100)
X3 <- rnorm(1000, 10, 10)
X4 <- rnorm(1000, -10, 90)
X_train <- cbind(X1, X2, X3, X4)
f <- function(X){
  # hardcoded coefficients
  beta0 <- 0.10
  beta1 <- 1.30
  beta2 <- 2.10
  beta3 <- 0.98
  beta4 <- 0.07
  Y <- 1:nrow(X)
  
  for(i in 1:nrow(X)){
    error <- runif(1, -10, 10)
    Y[i] <- beta0 + X[i,1]*beta1 + X[i,2]*beta2 + X[i,3]*beta3 + X[i,4]*beta4 + error
  }
  return(Y)
}
Y_train <- f(X_train)
data_train <- cbind(X_train, Y_train)


# generate external test set
X1 <- rnorm(1000, 0, 10)
X2 <- rnorm(1000, 0, 100)
X3 <- rnorm(1000, 10, 10)
X4 <- rnorm(1000, -10, 90)
X_test <- cbind(X1, X2, X3, X4)
Y_test <- f(X_test)


# create bootstrap samples
N_boot_samples <- 10000

# fit models to each bootstrap sample and extract coefficients
beta0_arr <- 1:N_boot_samples
beta1_arr <- 1:N_boot_samples
beta2_arr <- 1:N_boot_samples
beta3_arr <- 1:N_boot_samples
beta4_arr <- 1:N_boot_samples

for(i in 1:N_boot_samples){
  subsample <- data_train[sample(nrow(data_train), size = nrow(data_train), replace = TRUE), ] # select a subset of rows of the training data (with replacement)
  model <- lm(subsample[,5] ~ subsample[,1] + subsample[,2] + subsample[,3] + subsample[,4])  # fit a model to the subset
  coefs <- summary(model)$coefficients  # extract model coefficients
  beta0_arr[i] <- coefs[1]
  beta1_arr[i] <- coefs[2]
  beta2_arr[i] <- coefs[3]
  beta3_arr[i] <- coefs[4]
  beta4_arr[i] <- coefs[5]
}


# create distributions of coefficients (normal distribution assumed)
mu_beta0 <- mean(beta0_arr)
var_beta0 <- var(beta0_arr)

mu_beta1 <- mean(beta1_arr)
var_beta1 <- var(beta1_arr)

mu_beta2 <- mean(beta2_arr)
var_beta2 <- var(beta2_arr)

mu_beta3 <- mean(beta3_arr)
var_beta3 <- var(beta3_arr)

mu_beta4 <- mean(beta4_arr)
var_beta4 <- var(beta4_arr)


# make final model
beta0_final <- rnorm(1, mu_beta0, var_beta0)
beta1_final <- rnorm(1, mu_beta1, var_beta1)
beta2_final <- rnorm(1, mu_beta2, var_beta2)
beta3_final <- rnorm(1, mu_beta3, var_beta3)
beta4_final <- rnorm(1, mu_beta4, var_beta4)


final_model <- function(X){
  Y_pred <- beta0_final + X[,1]*beta1_final + X[,2]*beta2_final + X[,3]*beta3_final + X[,4]*beta4_final
  return(Y_pred)
}


# predict using final model
Y_pred <- final_model(X_test)

# measure predictiuon error
MSE <- 1/length(Y_pred) * sum((Y_test - Y_pred)^2)
MSE


