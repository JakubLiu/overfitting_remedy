X1 <- rnorm(1000, 0, 0.01)
X2 <- rnorm(1000, 0, 0.01)
X3 <- rnorm(1000, 10, 0.01)
X4 <- rnorm(1000, -10, 0.01)
X <- cbind(X1, X2, X3, X4)
f <- function(X){
  # hardcoded coefficients
  beta0 <- 0.10
  beta1 <- 1.30
  beta2 <- 2.10
  beta3 <- 0.98
  beta4 <- 0.07
  Y <- 1:nrow(X)
  
  for(i in 1:nrow(X)){
    error <- runif(1, -100, 100)
    Y[i] <- beta0 + X[i,1]*beta1 + X[i,2]*beta2 + X[i,3]*beta3 + X[i,4]*beta4 + error
  }
  return(Y)
}
Y <- f(X)
data_train <- data.frame(cbind(X,Y))

f_new <- function(X){
  # hardcoded coefficients
  beta0 <- 50.2
  beta1 <- 60.40
  beta2 <- 80.20
  beta3 <- 50.1
  beta4 <- 50.17
  Y <- 1:nrow(X)
  
  for(i in 1:nrow(X)){
    error <- runif(1, -100, 100)
    Y[i] <- beta0 + X[i,1]*beta1 + X[i,2]*beta2 + X[i,3]*beta3 + X[i,4]*beta4 + error
  }
  return(Y)
}
Y_new <- f_new(X)
data_test <- data.frame(cbind(X,Y_new))
X_test <- data_test[,c(1,2,3,4)]
Y_test <- data_test[,ncol(data_test)]

# fit and measure perofrmance of single model
model_orig <- lm(Y ~ X1 + X2 + X3 + X4, data = data_train)
Y_pred_orig <- predict(model_orig, X_test)
MSE_orig <- sum((Y_pred_orig - Y_test)^2) / length(Y_pred_orig)


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

Y_pred <- final_model(X_test)


MSE <- sum((Y_pred - Y_test)^2) / length(Y_pred)


par(mfrow = c(3,2))
hist(beta0_arr)
hist(beta1_arr)
hist(beta2_arr)
hist(beta3_arr)
hist(beta4_arr)

var(beta0_arr)
var(beta1_arr)
var(beta2_arr)
var(beta3_arr)
var(beta4_arr)

MSE
MSE_orig




# logistic regression

library(boot)

X1 <- rnorm(1000, 0, 0.01)
X2 <- rnorm(1000, 0, 0.01)
X3 <- rnorm(1000, 10, 0.01)
X4 <- rnorm(1000, -10, 0.01)
Y <- round(runif(1000, 0, 1))

data_full <- data.frame(cbind(X1, X2, X3, X4, Y))
k <- 0.7*nrow(data_full)

data_train <- data_full[1:k,]
data_test <- data_full[k:nrow(data_full),]
X_test <- data.frame(data_test[,c(1,2,3,4)])
Y_test <- data.frame(data_test[,ncol(data_test)])

model_orig <- glm(Y ~ X1, data = data_train, family = 'binomial')
Y_pred_orig <- round(predict(model_orig, data.frame(X_test[,1]), type="response"))


# below we need to create the bootstrap samples and create a final model
# given the coefficients of the final model we can make predictions like this:
# round(inv.logit(beta0 - beta1 * X_test[,1])) ----> the boot package must be loaded first
# to get out the coefficients of the final model run:
# summary(model_orig)$coefficients[1] ---> beta0
# summary(model_orig)$coefficients[2] --> beta1
