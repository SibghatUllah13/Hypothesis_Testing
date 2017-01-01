# Question 1,
set.seed(123)  # Setting seed for random

mu1 <- rnorm(1, 0, 1)  # Miu 1
mu2 <- rnorm(1, 0, 1)  # Miu 2
sig <- rexp(1, 3)      # Sigma (It is only one value)
x1 <- rnorm(10000, mu1, sig)   # X 1 distr.
y2 <- rnorm(10000, mu2, sig)   # Y 2 distr.
y1 <- rnorm(10000, mu1, sig)   # Y 1 distr.
x2 <- rnorm(10000, mu2, sig)   # X 2 distr.


# X2|X1 -------------------------------------------------------------------
xmu2_1 <- mu2 + cov(x2,x1)*(mean(x1)- mu1)/sig   # Miu 2|1. For computing X2 | X1 conditional distr.
xsig2_1 <- sig - cov(x2,x1)*cov(x1,x2)/sig  # Sig 2|1. For computing X2 | X1 conditional distr.
x2_1 <- rnorm(10000, xmu2_1, xsig2_1)       # X2 | X1

# Y1|Y2 -------------------------------------------------------------------
ymu1_2 <- mu1 + cov(y1,y2)*(mean(y2)-mu2)/sig     # Miu 1|2. For computing Y1 | Y2 conditional distr.
ysig1_2 <- sig - cov(y1,y2)*cov(y2,y1)/sig  # Sig 1|2. For computing Y1 | Y2 conditional distr.
y1_2 <- rnorm(10000, ymu1_2, ysig1_2)       # Y2 | Y1

joint = mu1*mu2*sig*x1*y2*x2_1*y1_2
hist(x1)


