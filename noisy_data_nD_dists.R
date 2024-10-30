library(MASS)

# functions that will be used later
MSE <- function(y_true, y_pred){
    res <- mean((y_true-y_pred)^2)
    return(res)
}

CovMatrix <- function(vector_of_vectors){
    sig <- matrix(rep(0,length(vector_of_vectors)*length(vector_of_vectors)),
                nrow = length(vector_of_vectors),
                ncol = length(vector_of_vectors))

    for(i in 1:length(vector_of_vectors)){
        for(j in 1:length(vector_of_vectors)){
            if(i == j){
                sig[i,j] <- var(vector_of_vectors[[i]])
            }
            else{
                sig[i,j] <- cov(vector_of_vectors[[i]], vector_of_vectors[[j]])
            }
        }
    }
    return(sig)
}


set.seed(444)
n <- 600
p <- 5
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
beta0_true <- 5
beta1_true <- 0.5
beta2_true <- 1.5
beta3_true <- 1.2
beta4_true <- 2
beta5_true <- -1.3
Y <- beta0_true + X[,1]*beta1_true + X[,2]*beta2_true + X[,3]*beta3_true + X[,4]*beta4_true + X[,5]*beta5_true
Y <- Y + rnorm(n,0,sd(Y)*0.05)  # add a noise that is 5% of the standars deviation of Y
data_full <- cbind(X,Y)

# train test split
k <- floor(nrow(data_full)*0.7)
data_train <- data_full[1:k, ]
data_test <- data_full[k:nrow(data_full), ]
X_train <- data_train[,1:ncol(data_train)-1]
Y_train <- data_train[,ncol(data_train)]
X_test <- data_test[,1:ncol(data_test)-1]
Y_test <- data_test[,ncol(data_test)]


model_orig<- lm(Y_train ~ X_train)
coefs <- summary(model_orig)$coefficients
y_hat_orig <- coefs[1] + X_test[,1]*coefs[2] + X_test[,2]*coefs[3] + X_test[,3]*coefs[4] + X_test[,4]*coefs[5] + X_test[,5]*coefs[6]

n_boot_samples <- 1000
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

# create multidimensional normal distribution
mu <- c(mean(beta0_est), mean(beta1_est),
        mean(beta2_est), mean(beta3_est),
        mean(beta4_est), mean(beta5_est))

covariance_matrix <- CovMatrix(list(beta0_est, beta1_est, beta2_est,
                                    beta3_est, beta4_est, beta5_est))


params <- mvrnorm(n = 1, mu = mu, Sigma = covariance_matrix)
beta0_from_dist <- params[1]
beta1_from_dist <- params[2]
beta2_from_dist <- params[3]
beta3_from_dist <- params[4]
beta4_from_dist <- params[5]
beta5_from_dist <- params[6]

final_model <- function(X){
    Y <- beta0_from_dist + X[,1]*beta1_from_dist + X[,2]*beta3_from_dist + X[,3]*beta3_from_dist + X[,4]*beta4_from_dist + X[,5]*beta5_from_dist
    return(Y)
}

y_hat_boot <- final_model(X_test)

MSE_orig <- MSE(Y_test, y_hat_orig)
MSE_boot <- MSE(Y_test, y_hat_boot)

MSE_orig
MSE_boot