library(data.table)

MSE <- function(y_true, y_pred){
    res <- mean((y_true-y_pred)^2)
    return(res)
}

data <- fread('/media/DANE/home/jliu/MASTER_THESIS/Customer Purchasing Behaviors.csv')
data$user_id <- NULL
data$region <- NULL

k <- floor(nrow(data)*0.7)
colnames(data)[length(colnames(data))] <- "y"
X <- data[,1:(ncol(data)-1)]
Y <- data$y

X_train <- as.data.frame(X[1:k,])
X_test <- as.data.frame(X[(k+1):nrow(X),])
Y_train <- as.data.frame(Y[1:k])
Y_test <- as.data.frame(Y[(k+1):length(Y)])


model_orig <- lm(Y_train.Y_train ~ ., data = cbind(Y_train, X_train))
coefs <- summary(model_orig)$coefficients
y_hat_orig <- coefs[1] + data_test[,1]*coefs[2] + data_test[,2]*coefs[3] + data_test[,3]*coefs[4] + data_test[,4]*coefs[5]


n_boot_samples <- 1000
beta0_est <- 1:n_boot_samples
beta1_est <- 1:n_boot_samples
beta2_est <- 1:n_boot_samples
beta3_est <- 1:n_boot_samples
beta4_est <- 1:n_boot_samples

for(i in 1:n_boot_samples){
    boot_sample <- data_train[sample(nrow(data_train), nrow(data_train), replace = TRUE), ]  # note that here I also bootstrap Y and I have only bootstrapped X, this was wrong
    model <- lm(y ~ ., data = boot_sample)
    boot_coefs <- summary(model)$coefficients
    beta0_est[i] <- boot_coefs[1]
    beta1_est[i] <- boot_coefs[2]
    beta2_est[i] <- boot_coefs[3]
    beta3_est[i] <- boot_coefs[4]
    beta4_est[i] <- boot_coefs[5]
}

mu_beta0 <- mean(beta0_est)
mu_beta1 <- mean(beta1_est)
mu_beta2 <- mean(beta2_est)
mu_beta3 <- mean(beta3_est)
mu_beta4 <- mean(beta4_est)

sd_beta0 <- sd(beta0_est)
sd_beta1 <- sd(beta1_est)
sd_beta2 <- sd(beta2_est)
sd_beta3 <- sd(beta3_est)
sd_beta4 <- sd(beta4_est)

# 0.555
alpha <- 1

beta0_from_dist <- rnorm(1, mean = mu_beta0, sd = sd_beta0*alpha)
beta1_from_dist <- rnorm(1, mean = mu_beta1, sd = sd_beta1*alpha)
beta2_from_dist <- rnorm(1, mean = mu_beta2, sd = sd_beta2*alpha)
beta3_from_dist <- rnorm(1, mean = mu_beta3, sd = sd_beta3*alpha)
beta4_from_dist <- rnorm(1, mean = mu_beta4, sd = sd_beta4*alpha)

final_model <- function(test_set){
    y_hat <- beta0_from_dist + test_set[,1]*beta1_from_dist + test_set[,2]*beta2_from_dist + test_set[,3]*beta3_from_dist + test_set[,4]*beta4_from_dist
    return(y_hat)
}

y_hat_boot <- final_model(data_test)
MSE_orig <- MSE(data_test$y, y_hat_orig)
MSE_boot <- MSE(data_test$y, y_hat_boot)


MSE_orig
MSE_boot