## first part
make.indicators <- function(x) {
  ux <- unique(x)
  mat1 <- matrix(x, nrow = length(x), ncol = length(ux))
  mat2 <- matrix(ux, nrow = length(x), ncol = length(ux), byrow = TRUE)
  (mat1 == mat2) *1}
y.1 <- c(5.0, 13.0, 7.2, 6.8, 12.8, 5.8, 9.5, 6.0, 3.8, 14.3, 1.8, 6.9, 4.7, 9.5)
y.2 <- c(0.9, 12.9, 2.6, 3.5, 26.6, 1.5, 13.0, 8.8, 19.5,2.5, 9.0, 13.1, 3.6, 6.9)
y.3 <- c(14.3, 6.9, 7.6, 9.8, 2.6, 43.5, 4.9, 3.5, 4.8, 5.6, 3.5, 3.9, 6.7)
basement.1 <- c(1,1,1,1, 1, 0, 1,1,1,0,1,1,1,1)
basement.2 <- c(0,1,1,0,1,1,1,1,1,0,1,1,1,0)
basement.3 <- c(1,0,1,0,1,1,1,1,1,1,1,1,1)
counties <- rep(1:3, c(length(y.1), length(y.2), length(y.3)))
y <- c(y.1, y.2, y.3)
x <- cbind(c(basement.1, basement.2, basement.3), make.indicators(counties))
x
class(x)
ls.out <- lsfit(x, log(y), intercept = F)
lsd <- ls.diag(ls.out)
x
y
ls.out


nsim <- 100000
n <- nrow(x)
k <- ncol(x)


sigma <- rep(NA, nsim)
beta <- array(NA, c(nsim, k))
for(i in 1:nsim) {
  sigma[i] <- lsd$std.dev*sqrt((n-k)/rchisq(1,n-k))
  beta[i,] <- ls.out$coef + (sigma[i]/lsd$std.dev)*lsd$std.err*t(chol(lsd$corr))%*%rnorm(k)}

output <- exp(cbind(beta[,2], beta[,1] + beta[,2], beta[,3], beta[,1] +
                      beta[,3], beta[,4], beta[,1] + beta[,4], beta[,1], sigma))
for(i in 1:ncol(output)) print(round(quantile(output[,i],c(.25,.5,.75)),1))


## second part

theta <- rbeta(nsim, 3, 13)
b <- rbinom(nsim, 1, theta)
logy.rep <- rnorm(nsim, beta[,3] + b*beta[,1], sigma)
y.rep <- exp(logy.rep)
print(round(quantile(y.rep, c(.025, .25, .5, .75, .975)), 1))
hist(y.rep[y.rep<40], yaxt = "n", breaks =0:40,
     xlab = "radon measurement (new house)", ced = 2)


## house in Blue Earth county
Xstar <- c(1,1,0,0)
Xstar <- t(Xstar)


#sampling a new y
ystar_samp <- rep(NA, numsamp)
for(i in 1:numsamp){
  xstarbeta <- Xstar%*%t(t(beta[i,]))
  ystar_samp[i] <- rnorm(1,
                         mean = )
}
ystar_samp
hist(ystar_samp)
quantile(ystar_samp, c(.50))
