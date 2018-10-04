

## Estimating binomial distribution. two outcomes, 20 coins, 100 replicate trials
## First 50% chance of heads then 60% chance of heads.
## Dist shifts to the right
hist(rbinom(100, 20, 0.5))
hist(rbinom(100, 20, 0.6))
## X-axis represents the number of successful results (heads)
## y-axis is the number of trials with that level of success
## A result of 16 would indicate a very rare outcome or a 
## rigged coin

hist(rnorm(10000, mean = 50.5, sd = 5.5))
hist(rnorm(10000, mean = 50.5, sd = 50.5))

x <- rnorm(10000, mean = 50.5, sd = 5.5)
x_sample_means <- NULL
for(i in 1:1000){
  x_samp <- sample(x, 20, replace=FALSE)
  x_sample_means[i] <- mean(x_samp)
}
hist(x_sample_means)
x_sample_means[1]
x_samp
x_samp_z <- scale(x_samp)
x_samp_z <- (x_samp - mean(x_samp))/sd(x_samp)
x_samp_z
mean(x_samp_z)
sd(x_samp_z)
hist(x_samp_z)


x.lognorm <- log(x)
hist(x.lognorm)
x_sample_means <- NULL
for(i in 1:1000){
  x_samp <- sample(x, 20, replace=FALSE)
  x_sample_means[i] <- mean(x_samp)
}
hist(x_sample_means)
## The distribution of the lognormal means looks normally distributed