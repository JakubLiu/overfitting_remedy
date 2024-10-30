library(faux)

# for multiple regression with 3 predictors
final_model_3_pred <- function(x1, x2, x3, beta0_from_dist, beta1_from_dist, beta2_from_dist, beta3_from_dist){
  y <- x1*beta1_from_dist + x2*beta2_from_dist + x3*beta3_from_dist + beta0_from_dist
  return(y)
}

MSE <- function(y_true, y_hat){
  res <- sum((y_true - y_hat)^2)/length(y_true)
}

set.seed(444)

sigma_high <- matrix(c(1, 0.8, 0.75, 0.85,
                       0.8, 1, 0.7, 0.9,
                       0.75, 0.7, 1, 0.8,
                       0.85, 0.9, 0.8, 1), nrow = 4)

sigma_low <- matrix(c(1, 0.1, 0.15, 0.05,
                      0.1, 1, 0.2, 0.1,
                      0.15, 0.2, 1, 0.05,
                      0.05, 0.1, 0.05, 1), nrow = 4)

data_high <- rnorm_multi(
                      n = 10000,
                      mu = c(10,20, 100 ,100),
                      sd = c(5,7,7,9),
                      r = sigma_high,
                      varnames = c("X1", "X2", "X3", "Y")
)

data_low <- rnorm_multi(
                      n = 10000,
                      mu = c(10,20, 100 ,100),
                      sd = c(5,7,7,9),
                      r = sigma_low,
                      varnames = c("X1", "X2", "X3", "Y")
)

data_full <- rbind(data_high, data_low) # combine
data_full <- data_full[sample(nrow(data_full)), ]  # shuffle  
k <- floor(nrow(data_full)*0.7)  # train test split
data_train <- data_full[1:k,]
data_test <- data_full[k:nrow(data_full),]

model_orig <- lm(Y ~ X1 + X2 + X3, data = data_train)
y_pred_orig <- predict(model_orig, data_test[,c(1,2,3)])

n_boot_samples <- 1000

beta0_est <- 1:n_boot_samples
beta1_est <- 1:n_boot_samples
beta2_est <- 1:n_boot_samples
beta3_est <- 1:n_boot_samples

MSE_compare <- function(n_boot_samples, alpha){  

  for(i in 1:n_boot_samples){
      boot_sample <- data_train[sample(nrow(data_train),size = nrow(data_train),replace = TRUE), ]
      model <- lm(Y ~ X1 + X2 + X3, data = boot_sample)
      beta0_est[i] <- summary(model)$coefficients[1]
      beta1_est[i] <- summary(model)$coefficients[2]
      beta2_est[i] <- summary(model)$coefficients[3]
      beta3_est[i] <- summary(model)$coefficients[4]
    }


  mean_beta0 <- mean(beta0_est)
  sd_beta0 <- sd(beta0_est)
    
  mean_beta1 <- mean(beta1_est)
  sd_beta1 <- sd(beta1_est)
    
  mean_beta2 <- mean(beta2_est)
  sd_beta2 <- sd(beta2_est)
    
  mean_beta3 <- mean(beta3_est)
  sd_beta3 <- sd(beta3_est)
    
    
  beta0_from_dist <- rnorm(n = 1, mean = mean_beta0, sd = sd_beta0*alpha)
  beta1_from_dist <- rnorm(n = 1, mean = mean_beta1, sd = sd_beta1*alpha)
  beta2_from_dist <- rnorm(n = 1, mean = mean_beta2, sd = sd_beta2*alpha)
  beta3_from_dist <- rnorm(n = 1, mean = mean_beta3, sd = sd_beta3*alpha)
    
  y_pred_boot <- final_model_3_pred(data_test$X1, data_test$X2, data_test$X3, beta0_from_dist,
                  beta1_from_dist, beta2_from_dist, beta3_from_dist)

  MSE_orig <- MSE(data_test$Y, y_pred_orig)
  MSE_boot <- MSE(data_test$Y, y_pred_boot)
  list('MSE_orig' = MSE_orig, 'MSE_boot' = MSE_boot, 'reduction' = MSE_orig - MSE_boot)
}

N <- seq(from = 1000, to = 1000000, by = 500)
a <- seq(from = 0.1, to = 50, by = 0.5)
param_grid <- data.frame(matrix(rep(0, length(N)*length(a)), nrow = length(N), ncol = length(a)))
rownames(param_grid) <- N
colnames(param_grid) <- a

for(i in 1:length(N)){
  for(j in 1:length(a)){
    reduct <- MSE_compare(N[i], a[j])$reduction
    param_grid[i,j] <- reduct
    out <- paste0("N: ", N[i], " alpha: ", a[j], " reduction: ", reduct)
    print(out)
 }
}


write.csv(reduct, '/media/DANE/home/jliu/MASTER_THESIS/MSE_reduction_big_alpha.csv', row.names=TRUE)


