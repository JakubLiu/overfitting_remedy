set.seed(444)

#define necessary functions
Predict <- function(newdata, coefs){
    p <- dim(coefs)[1]
    betas <- 1:(p-1)
    beta0 <- coefs[1]

    for(i in 2:p){
        betas[i-1] <- coefs[i]
    }
    
    Y_hat <- newdata %*% betas
    Y_hat <- Y_hat + beta0
    return(Y_hat)
}

MSE <- function(y_true, y_pred){
    res <- sum((y_true-y_pred)^2)
    res <- res/length(y_true)
    return(res)
}

# prepare data
n <- 200
p <- 50
X <- matrix(rnorm(n*p,0,1), nrow = n, ncol = p)
true_coefficients <- matrix(runif(p, -5, 5), nrow = p, ncol = 1)
true_intercept <- runif(1,-5,5)
zero_idx <- unique(round(runif(15,1,15)))  # select random (at most) 15 coefficients to be set to almost 0 (not correlated with the response)
true_coefficients[zero_idx] <- runif(1,0,0.001)
Y <- X %*% true_coefficients
Y <- Y + true_intercept
Y <- Y + rnorm(n,0,sd(Y)*1.0)
data <- cbind(X,Y)
k <- floor(nrow(data)*0.7)
data_train <- data[1:k,]
data_test <- data[(k+1):nrow(data),]

# original model
model_orig <- lm(data_train[,ncol(data_train)] ~ data_train[,1:(ncol(data_train)-1)])
coefs_orig <- summary(model_orig)$coefficients
y_hat_orig <- Predict(data_test[,1:(ncol(data_test)-1)], coefs_orig)
MSE_orig <- MSE(data_test[,ncol(data_test)], y_hat_orig)


# bootstrap model
n_boot_samples <- 10000
coefs <- matrix(rep(0,n_boot_samples*(p+1)), nrow = n_boot_samples, ncol = (p+1))

# get coefficient estimates from bootstrap samples
for(i in 1:n_boot_samples){
    data_train_boot <- data_train[sample(1:nrow(data_train), nrow(data_train), replace = T),]
    model_boot <- lm(data_train_boot[,ncol(data_train_boot)] ~ data_train_boot[,1:(ncol(data_train_boot)-1)])
    coefs_boot <- summary(model_boot)$coefficients

    for(j in 1:(p+1)){
        coefs[i,j] <- coefs_boot[j]
    }
}

# sample estimates
coefs_final <- matrix(1:(p+1),nrow = (p+1), ncol = 1)

alpha <- 0.1   # hyperparameter (empirically chosen optimal value: 0.1)

for(j in 1:(p+1)){
    mu <- mean(coefs[,j])
    sigma <- sd(coefs[,j])*alpha
    sampled_coef <- rnorm(1,mu,sigma)
    coefs_final[j,] <- sampled_coef
}

# predict using bootstrapped model
y_hat_boot <- Predict(data_test[,1:(ncol(data_test)-1)], coefs_final)
MSE_boot <- MSE(data_test[,ncol(data_test)], y_hat_boot)

MSE_orig
MSE_boot
