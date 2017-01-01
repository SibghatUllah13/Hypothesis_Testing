
# Gaussian Distribution Comparison ----------------------------------------
alpha = 2
N_seq = c(10,50,100,500,1000)
Bias_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Bias_Matrix) = c("Sample Mean", "Median of Mean")
Variance_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Variance_Matrix) = c("Sample Mean", "Median of Mean")

MSE_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(MSE_Matrix) = c("Sample Mean", "Median of Mean")
Tail_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Tail_Matrix) = c("Sample Mean", "Median of Mean")


median_of_means=function (x, optimal_block_size)
{
  mean_matrix = matrix(NA,nrow = length(x)/optimal_block_size, ncol=1)
  for (j in 1:length(x)/optimal_block_size)
  {
    mean_matrix[j]=mean(x[optimal_block_size*j])
  }
  out <- median(mean_matrix)
  
}

for (i in 1:length(N_seq))
{
  optimal_block_size= 2
  B = 1000
  est1 = matrix(NA,nrow = B ,ncol = 1)
  est2 = matrix(NA,nrow = B ,ncol = 1)
  for (b in 1:B)
  {
    actual_mean = 3.2
    x = rnorm(N_seq[i],actual_mean,1)
    est1[b,1] = sum(x)/length(x) #estimator1 (Sample Mean)
    est2[b,1] = median_of_means(x,optimal_block_size)  #estimator2 (Median of Samples)
  }
  
  Bias_Matrix[i,1] = mean(est1)-3.2  #E(estimated_theta)-actual_theta
  Bias_Matrix[i,2] = mean(est2)-3.2  #E(estimated_theta)-actual_theta
  
  Variance_Matrix[i,1] = mean((est1)^2)-(mean(est1)^2) #E[x2]-(E[x])2
  Variance_Matrix[i,2] = mean((est2)^2)-(mean(est2)^2) #E[x2]-(E[x])2
  
  MSE_Matrix[i,1] = (Variance_Matrix[i,1])+((Bias_Matrix[i,1])^2) #MSE=var+(bias)2
  MSE_Matrix[i,2] = (Variance_Matrix[i,2])+((Bias_Matrix[i,2])^2) #MSE=var+(bias)2
  
  # Assuming t=2
  t=2
  Tail_Matrix[i,1] <- (var(est1))/(t^2)
  Tail_Matrix[i,2] <- (var(est2))/(t^2)
  }



cat("--- Bias Comparison ----------- \n")
Bias_Matrix
cat("--- Variance Comparison ----------- \n")
Variance_Matrix
cat("--- MSE Comparison ----------- \n")
MSE_Matrix
cat("--- Tail Prob's Comparison ----------- \n")
Tail_Matrix


# Student Distribution Comparison ----------------------------------------
alpha = 2
N_seq = c(10,50,100,500,1000)
Bias_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Bias_Matrix) = c("Sample Mean", "Median of Mean")
Variance_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Variance_Matrix) = c("Sample Mean", "Median of Mean")

MSE_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(MSE_Matrix) = c("Sample Mean", "Median of Mean")
Tail_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Tail_Matrix) = c("Sample Mean", "Median of Mean")


median_of_means=function (x, optimal_block_size)
{
  mean_matrix = matrix(NA,nrow = length(x)/optimal_block_size, ncol=1)
  for (j in 1:length(x)/optimal_block_size)
  {
    mean_matrix[j]=mean(x[optimal_block_size*j])
  }
  out <- median(mean_matrix)
  
}

for (i in 1:length(N_seq))
{
  optimal_block_size= 2
  B = 1000
  est1 = matrix(NA,nrow = B ,ncol = 1)
  est2 = matrix(NA,nrow = B ,ncol = 1)
  for (b in 1:B)
  {
    actual_mean = 0 #Since DF>3 in t-dist, mean equals to 0
    x = rt(N_seq[i],3)
    est1[b,1] = sum(x)/length(x) #estimator1 (Sample Mean)
    est2[b,1] = median_of_means(x,optimal_block_size)  #estimator2 (Median of Samples)
  }
  
  Bias_Matrix[i,1] = mean(est1)-3.2  #E(estimated_theta)-actual_theta
  Bias_Matrix[i,2] = mean(est2)-3.2  #E(estimated_theta)-actual_theta
  
  Variance_Matrix[i,1] = mean((est1)^2)-(mean(est1)^2) #E[x2]-(E[x])2
  Variance_Matrix[i,2] = mean((est2)^2)-(mean(est2)^2) #E[x2]-(E[x])2
  
  MSE_Matrix[i,1] = (Variance_Matrix[i,1])+((Bias_Matrix[i,1])^2) #MSE=var+(bias)2
  MSE_Matrix[i,2] = (Variance_Matrix[i,2])+((Bias_Matrix[i,2])^2) #MSE=var+(bias)2
  
  # Assuming t=2
  t=2
  Tail_Matrix[i,1] <- (var(est1))/(t^2)
  Tail_Matrix[i,2] <- (var(est2))/(t^2)
}



cat("--- Bias Comparison ----------- \n")
Bias_Matrix
cat("--- Variance Comparison ----------- \n")
Variance_Matrix
cat("--- MSE Comparison ----------- \n")
MSE_Matrix
cat("--- Tail Prob's Comparison ----------- \n")
Tail_Matrix


# Binomial Distribution Comparison ----------------------------------------
alpha = 2
N_seq = c(10,50,100,500,1000)
Bias_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Bias_Matrix) = c("Sample Mean", "Median of Mean")
Variance_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Variance_Matrix) = c("Sample Mean", "Median of Mean")

MSE_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(MSE_Matrix) = c("Sample Mean", "Median of Mean")
Tail_Matrix = matrix(NA, nrow = length(N_seq), ncol = 2)
colnames(Tail_Matrix) = c("Sample Mean", "Median of Mean")


median_of_means=function (x, optimal_block_size)
{
  mean_matrix = matrix(NA,nrow = length(x)/optimal_block_size, ncol=1)
  for (j in 1:length(x)/optimal_block_size)
  {
    mean_matrix[j]=mean(x[optimal_block_size*j])
  }
  out <- median(mean_matrix)
  
}

for (i in 1:length(N_seq))
{
  optimal_block_size= 2
  B = 1000
  est1 = matrix(NA,nrow = B ,ncol = 1)
  est2 = matrix(NA,nrow = B ,ncol = 1)
  for (b in 1:B)
  {
    actual_mean = 10000 * 0.35
    x = rbinom(N_seq[i],10000,0.35)
    est1[b,1] = sum(x)/length(x) #estimator1 (Sample Mean)
    est2[b,1] = median_of_means(x,optimal_block_size)  #estimator2 (Median of Samples)
  }
  
  Bias_Matrix[i,1] = mean(est1)-3.2  #E(estimated_theta)-actual_theta
  Bias_Matrix[i,2] = mean(est2)-3.2  #E(estimated_theta)-actual_theta
  
  Variance_Matrix[i,1] = mean((est1)^2)-(mean(est1)^2) #E[x2]-(E[x])2
  Variance_Matrix[i,2] = mean((est2)^2)-(mean(est2)^2) #E[x2]-(E[x])2
  
  MSE_Matrix[i,1] = (Variance_Matrix[i,1])+((Bias_Matrix[i,1])^2) #MSE=var+(bias)2
  MSE_Matrix[i,2] = (Variance_Matrix[i,2])+((Bias_Matrix[i,2])^2) #MSE=var+(bias)2
  
  # Assuming t=2
  t=2
  Tail_Matrix[i,1] <- (var(est1))/(t^2)
  Tail_Matrix[i,2] <- (var(est2))/(t^2)
}



cat("--- Bias Comparison ----------- \n")
Bias_Matrix
cat("--- Variance Comparison ----------- \n")
Variance_Matrix
cat("--- MSE Comparison ----------- \n")
MSE_Matrix
cat("--- Tail Prob's Comparison ----------- \n")
Tail_Matrix



# Q2-Part2 ----------------------------------------------------------------

# Load the data
load("sp500.RData")
# Now some diagnostic plot
par(mfrow = c(1,2))
hist(sp.ind, prob = T, col = "pink", border = "white", breaks = 50,
     xlab = "S&P500 Index", main = "S&P500 log-returns")
lines(density(sp.ind), col = "orchid", lwd = 2)
# Compare the empirical quantiles with the normal quantiles at the
# same level: we see heavy tails and a bit of skewness
qqnorm(sp.ind, pch = 19, cex = .5)
qqline(sp.ind, col = "red")

# Nonparametric Boostrap
data = sp.ind[1:2700] #Getting the Data, Ommitting the last 80 rows (Just to Make Block Size Simple)
n = length(data) #sample size
B = 1000
N_seq = c(5,10,20,50,100) #Block Sizes
MSE_Boot = matrix(NA, nrow = length(N_seq), ncol = 1)
colnames(MSE_Boot) = c("MSE")
for (i in 1:length(N_seq))
{
  a.nboot = rep (NA,B)
  true_mean = mean(data)
  for (b in 1:B)
  {
    idx <- sample(1:n, replace = T)
    x.boot <- data[idx] #Empirical CDF
    a.nboot = median_of_means(x.boot,N_seq[i])
  } 
  bias = mean(a.nboot)-true_mean
  var = mean((a.nboot)^2) - (mean(a.nboot)^2)
  MSE_Boot [i] = var+(bias)^2
}

cat("--- MSE, WRT to N----------- \n")
print (MSE_Boot)
cat("--- Smallest MSE is when Block Size = ----------- \n")
print (N_seq[which.min(MSE_Boot)])  
