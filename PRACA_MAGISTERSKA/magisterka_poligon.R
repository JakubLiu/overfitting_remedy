library(faux)

# dataset with high correlation between y and x
data_high_cor <- rnorm_multi(
                      n = 10000,
                      mu = c(10,20),
                      sd = c(5,7),
                      r = c(0.9),
                      varnames = c("X", "Y")
)

# dataset with low correlation between y and x
data_low_cor <- rnorm_multi(
                      n = 10000,
                      mu = c(10,20),
                      sd = c(5,7),
                      r = c(0.2),
                      varnames = c("X", "Y")
)


data_full <- rbind(data_high_cor, data_low_cor)
View(data_full)
data_full <- data_full[sample(nrow(data_full)), ]   # shuffle the rows of the dataset
View(data_full)





model_orig <- lm(Y~X, data = testing_data)
summary(model_orig)
summary(model_orig)$coefficients[2]

n_boot_samples <- 10000
beta0_est <- 1:n_boot_samples
beta1_est <- 1:n_boot_samples

for(i in 1:n_boot_samples){
  boot_sample <- testing_data[sample(nrow(testing_data), size = nrow(testing_data), replace = TRUE), ]
  model <- lm(Y~X, data = boot_sample)
  beta0_est[i] <- summary(model)$coefficients[1]
  beta1_est[i] <- summary(model)$coefficients[2]
}

mean_beta0 <- mean(beta0_est)
sd_beta0 <- sd(beta0_est)
mean_beta1 <- mean(beta1_est)
sd_beta1 <- sd(beta1_est)

beta0_from_dist <- rnorm(n = 1, mean = mean_beta0, sd = sd_beta0)
beta1_from_dist <- rnorm(n = 1, mean = mean_beta1, sd = sd_beta1)

final_model <- function(x){
  y <- x*beta1_from_dist + beta0_from_dist
  return(y)
}

R2 <- function(y_true, y_pred){
  mu_y_true <- mean(y_true)
  RSS <- 0
  TSS <- 0
  for(i in 1:length(y_true)){
    RSS <- RSS + (y_true[i] - y_pred[i])^2
    TSS <- TSS + (y_true[i] - mu_y_true)^2
  }
  R2 <- 1 - RSS/TSS
  return(R2)
  
}

y_hat <- final_model(testing_data$X)


R2 <- R2(testing_data$Y, y_hat)