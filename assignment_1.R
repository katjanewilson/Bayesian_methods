#####
#Question 1
#####

dnorm(1,1,2)
0.199*0.5



((0.5*dnorm(1,1,2))/(0.5*dnorm(1,1,2) + 0.5*dnorm(1,2,2)))

#####
#Question 2
#####

t = 0
patient_arrival_times = list()
# look up how to hard code a queue in R


#####
#Question 3
#####

n <- 2
y<- 2
alpha <- 1
beta <- 5

postervariance_functions <- function(n, y) {
  x = ((1+y)/(2+n))*((1+n-y)/(2+n))*(1/(3+n))
  return(x)
}
priorvariance_function <- function(a, b) {
  denominator = (((a+b)*(a+b))*(a+b+a))
  x = (a*b)/denominator
  return(x)
}

#example of a small n, where posterior variance is higher
postervariance_functions(n= 4,y=4)
priorvariance_function(1,8)

#example of a small n, where posterior variance is higher
postervariance_functions(n= 3,y=3)
priorvariance_function(2,8)

#example of a large n, where posterior variance is lower
postervariance_functions(n= 200,y=2)
priorvariance_function(1,5)

#####
#Question 4
#####

post_variance_function = function(n) {
  x <- (1/ (   (1/(40*40))  + (n/(20*20))  ) )
  return(x)
}
post_predictive_variance_function = function(n) {
  x <- (1/ (   (1/(40*40))  + (n/(20*20))  ) ) + (20*20)
  return(x)
}
sqrt(post_variance_function(10))
sqrt(post_predictive_variance_function(10))

sqrt(post_variance_function(100))
sqrt(post_predictive_variance_function(100))

#####
#Question 5
#####

#plot the prior density function
theta <- seq(0,1,.001)
dens <- dbeta(theta, 1, .67)
plot(theta, dens, xlim = c(0,1), ylim = c(0,3),
     type = "l", xlab = "theta", ylab = "", xaxs = "i",
     yaxs = "i", yaxt = "n", bty = "n", cex = 2)

#draw the posterior density function
theta <- seq(0,1,.001)
dens <- dbeta(theta, 651, 350.67)
cond <- dens/max(dens) > 0.001
plot(theta[cond], dens[cond],
     type = "l", xlab = "theta", ylab = "", xaxs = "i",
     yaxs = "i", yaxt = "n", bty = "n", cex = 2)


#####
#Question 6
#####

dens <- function(y,th) {
  dens0 <- NULL
  for (i in 1:length(th))
    dens0 <- c(dens0, prod (dcauchy(y, th[i] ,1)))
  dens0}
y <- c(-2, -1,0,1.5, 2.5)
step <- .01
theta <- seq(step/2, 1-step/2, step)
dens.unnorm <- dens(y,theta)
dens.norm <- dens.unnorm/(step*sum(dens.unnorm))
plot(theta, dens.norm, ylim=c(0,1.1*max(dens.norm)),
     type = "l", xlab = "theta", ylab = "normalized density",
     xaxs = "i", yaxs = "i", cex = 2)


#b
thetas <- sample (theta, 1000, step *dens.norm, replace = TRUE)
hist(thetas, xlab = "theta", yaxt = "n",
     breaks = seq(0,1,.05), cex = 2)

#c
y6 <- rcauchy(length(thetas), thetas, 1)
hist(y6, xlab = "new observations", yaxt = "n", nclass = 100, cex =2)

#####
#Question 7
#####

#a

data <- read.table("data/planes.txt",header=T)
attach(data)

#b

theta <- rgamma(1000, 238)/10
y1986 <- rpois(1000,theta)
print(sort(y1986)[c(25,976)])

#####
#Question 9
#####

#a
mu.c <- 1.013 + (0.24/sqrt(32))*rt(1000,31)
mu.t <- 1.173 + (0.20/sqrt(36))*rt(1000,35)
dif <- mu.t - mu.c
hist(dif, xlab = "mu_t - mu_c", yaxt = "n",
     breaks = seq(-.1, .4, .02), cex = 2)


#b use the samples to plot a histogram of the differences,
#and calculate a 95% posterior interval
print(sort(dif[c(25,976)]))

#####
#Question 10
#####

#use grid sampling





