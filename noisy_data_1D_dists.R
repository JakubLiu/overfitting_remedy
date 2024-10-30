set.seed(444)
n <- 6000
p <- 5
X <- matrix(rnorm(n*p,0,1), nrow = n, ncol = p)
beta0_true <- 5  
beta1_true <- 0.5
beta2_true <- 1.5
beta3_true <- -0.5
beta4_true <- -1.5
beta5_true <- 2
Y <- beta0_true + X[,1]*beta1_true + X[,2]*beta2_true + X[,3]*beta3_true + X[,4]*beta4_true + X[,5]*beta5_true
Y <- Y + rnorm(n,0,sd(Y)*0.5)  # add a noise that is 5% of the standars deviation of Y
data_full <- cbind(X,Y)

# train test split
k <- floor(nrow(data_full)*0.7)
data_train <- data_full[1:k, ]
data_test <- data_full[k:nrow(data_full), ]
X_train <- data_train[,1:ncol(data_train)-1]
Y_train <- data_train[,ncol(data_train)]
X_test <- data_test[,1:ncol(data_test)-1]
Y_test <- data_test[,ncol(data_test)]

MSE <- function(y_true, y_pred){
    res <- mean((y_true-y_pred)^2)
    return(res)
}

model_orig<- lm(Y_train ~ X_train)
coefs <- summary(model_orig)$coefficients
y_hat_orig <- coefs[1] + X_test[,1]*coefs[2] + X_test[,2]*coefs[3] + X_test[,3]*coefs[4] + X_test[,4]*coefs[5] + X_test[,5]*coefs[6]

n_boot_samples <- 1000000
beta0_est <- 1:n_boot_samples
beta1_est <- 1:n_boot_samples
beta2_est <- 1:n_boot_samples
beta3_est <- 1:n_boot_samples
beta4_est <- 1:n_boot_samples
beta5_est <- 1:n_boot_samples

for(i in 1:n_boot_samples){
    boot_sample <- X_train[sample(nrow(X_train), nrow(X_train), replace = TRUE), ]
    model <- lm(Y_train ~ boot_sample)
    boot_coefs <- summary(model)$coefficients
    beta0_est[i] <- boot_coefs[1]
    beta1_est[i] <- boot_coefs[2]
    beta2_est[i] <- boot_coefs[3]
    beta3_est[i] <- boot_coefs[4]
    beta4_est[i] <- boot_coefs[5]
    beta5_est[i] <- boot_coefs[6]
}

mu_beta0 <- mean(beta0_est)
mu_beta1 <- mean(beta1_est)
mu_beta2 <- mean(beta2_est)
mu_beta3 <- mean(beta3_est)
mu_beta4 <- mean(beta4_est)
mu_beta5 <- mean(beta5_est)

sd_beta0 <- sd(beta0_est)
sd_beta1 <- sd(beta1_est)
sd_beta2 <- sd(beta2_est)
sd_beta3 <- sd(beta3_est)
sd_beta4 <- sd(beta4_est)
sd_beta5 <- sd(beta5_est)

alpha <- 1

beta0_from_dist <- rnorm(1, mean = mu_beta0, sd = sd_beta0*alpha)
beta1_from_dist <- rnorm(1, mean = mu_beta1, sd = sd_beta1*alpha)
beta2_from_dist <- rnorm(1, mean = mu_beta2, sd = sd_beta2*alpha)
beta3_from_dist <- rnorm(1, mean = mu_beta3, sd = sd_beta3*alpha)
beta4_from_dist <- rnorm(1, mean = mu_beta4, sd = sd_beta4*alpha)
beta5_from_dist <- rnorm(1, mean = mu_beta5, sd = sd_beta5*alpha)

final_model <- function(X){
    Y <- beta0_from_dist + X[,1]*beta1_from_dist + X[,2]*beta2_from_dist + X[,3]*beta3_from_dist + X[,4]*beta4_from_dist + X[,5]*beta5_from_dist
    return(Y)
}

y_hat_boot <- final_model(X_test)


MSE_orig <- MSE(Y_test, y_hat_orig)
MSE_boot <- MSE(Y_test, y_hat_boot)

print(MSE_orig)
print(MSE_boot)