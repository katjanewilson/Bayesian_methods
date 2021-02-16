#####
#Question 1
#####

dnorm(1,1,2)
0.199*0.5



((0.5*dnorm(1,1,2))/(0.5*dnorm(1,1,2) + 0.5*dnorm(1,2,2)))

#####
#Question 2
#####

## the conjucate prior for the poisson likelihood is the gamma


## time htat patients appear (exponential distribution with 10 minutes)
set.seed(220)

arrival.times <- c()
num.patients<- length(arrival.times)
next.pateitn <- 0
while( arrival.times[length(arrival.times)] < 420) {
  next.patient <- next.patient +rexp(1,.1) #.1 because the mean is 10, 
  #and the parameter for exponential is 1/mean
  arrival.times <- c(arrival.times, next.patient)
  arrival.times[length(arrival.times)]
  num.patients <- length(arrival.times)
}

arrival.time[c(-1, -length(arrival.times))]

#initialize things and then run through a loo

doc.vail <- c(0,0,0)
# how much they are waiting for the next doctor
#total waiting time
#only waiting time
#only want to average the patients who are waiting
#that is the total waiting, but only adding up over 0

### part a
mean(only.waiting.time)
num.waiting <-length(only.waiting.time)

office.close <- arrival.times[num.patients] + time.with.doc[num.patients]


## b put it into a loop and loop

#initialize a few things to store those results, and 
#then store those results, and cmoe up with a 95% interval


sim.num.patients <- c() 

j <- 0

while(j<100) {
  
  all of this again
  
  
  at end: j = j+1
  
  #store these at the each of the end of these loops
}

then print out:
  sim.num.patients

theta <- rexp(1,10)
theta
# then, a poisson distribution for wait time by 7 hours times 60 minutes, # of patients
patients <- rpois(1, theta*420)
patients
#amount of time each patient waits (expectation 10 minutes?)
theta.doc <- runif(103,1,10)
theta.doc
#amount of time the doctor spends with each patient (uniform, 5 to 20)
theta.doc <- runif(103,5,20)
theta.doc
#how to simulate the wait time?

##b simulate this 100 times and calculate hte median and 95 percent CI for each
# of the four quantities calculated in a

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
sumfatal <- sum(fatal)
n <- length(fatal)
##### looking at different Gamma priors #####
theta <- ppoints(1000)*40
gammaprior1 <- dgamma(theta,shape=0,rate=0)
minplot <- min(gammaprior1)
maxplot <- max(gammaprior1)
plot(theta,gammaprior1,type="l",ylim=c(minplot,maxplot),lwd=2,ylab="")
#set the prior to be stronger
gammaprior1 <- dgamma(theta,shape=200,rate=10)
plot(theta,gammaprior1,type="l",ylim=c(minplot,maxplot),lwd=2,ylab="")
#set the prior to be  off
gammaprior1 <- dgamma(theta,shape=40,rate=10)
plot(theta,gammaprior1,type="l",ylim=c(minplot,maxplot),lwd=2,ylab="")
## now plot the posterior distribution
gammaposterior1 <- dgamma(theta,shape=(sumfatal+0),rate=(n+0))
minplot <- min(gammaposterior1)
maxplot <- max(gammaposterior1)
hist(fatal,xlim=c(0,40),ylim=c(minplot,maxplot),prob=T,col="gray",xlab="",ylab="",main="")
par(new=T)
plot(theta,gammaposterior1,type="l",xlim=c(0,40),ylim=c(minplot,maxplot),lwd=2,xlab="",ylab="",main="")
legend("topright",c("Gamma(0,0)"),col=c(1:4),lwd=2)


#b

###use samples from the posterior distribution to obtain samples from the
#posterior predictive distribution
gammaposterior1 <- dgamma(theta,shape=(sumfatal+0),rate=(n+0))
minplot <- min(gammaposterior1)
maxplot <- max(gammaposterior1)
hist(fatal,xlim=c(0,40),ylim=c(minplot,maxplot),prob=T,col="gray",xlab="",ylab="",main="")
par(new=T)
plot(theta,gammaposterior1,type="l",xlim=c(0,40),ylim=c(minplot,maxplot),lwd=2,xlab="",ylab="",main="")
legend("topright",c("Gamma(0,0)"),col=c(1:4),lwd=2)

##compute a 95% interval

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


############################################################
### Grid Search and Grid Sample: Poisson Planes Dataset ####
############################################################

## input data:  
data <- read.table("data/planes.txt",skip=1)
y <- data[,2]
t <- data[,1]-1976
n <- length(y)
hist(y)
plot(t,y,pch=19)

## graphing posterior over range of alpha and beta:
posteriorplanes <- function(alpha,beta){
  logpost <- -Inf
  if (alpha + beta*max(t) > 0){
    logpost <- 0
    for (i in 1:n){
      logpost <- logpost + y[i]*log(alpha+beta*t[i])
      logpost <- logpost - (alpha+beta*t[i])
    }
  }
  logpost
}

numgrid <- 100
alpharange <- ppoints(numgrid)*20   # alpha between 0 and 20
betarange <- ppoints(numgrid)*6  # beta between 0 and 6

numgrid <- 100
alpharange <- ppoints(numgrid)*20+20   # alpha between 20 and 40
betarange <- ppoints(numgrid)*6-3  # beta between -3 and 3
full <- matrix(NA,nrow=numgrid,ncol=numgrid)
for (i in 1:numgrid){
  for (j in 1:numgrid){
    full[i,j] <- posteriorplanes(alpharange[i],betarange[j])
  }
}
full <- exp(full - max(full))
full <- full/sum(full)
contour(alpharange,betarange,full,xlab="alpha",ylab="beta",drawlabels=F)

## calculating probabilities for grid sampler:

alphamarginal <- rep(NA,numgrid)
for (i in 1:numgrid){
  alphamarginal[i] <- sum(full[i,])
}
betaconditional <- matrix(NA,nrow=numgrid,ncol=numgrid)
for (i in 1:numgrid){
  for (j in 1:numgrid){
    betaconditional[i,j] <- full[i,j]/sum(full[i,])
  }
}

## plotting marginal distribution of alpha
par(mfrow=c(1,1))
plot(alpharange,alphamarginal,type="l",main="marginal dist. of alpha")

## plotting conditional distribution of beta given alpha
alpharange[25]
alpharange[50]
alpharange[75]
par(mfrow=c(3,1))
plot(betarange,betaconditional[25,],type="l",main="dist. of beta for alpha = 24.9")
plot(betarange,betaconditional[50,],type="l",main="dist. of beta for alpha = 29.9")
plot(betarange,betaconditional[75,],type="l",main="dist. of beta for alpha = 34.9")

## sampling grid values:

alpha.samp <- rep(NA,10000)
beta.samp <- rep(NA,10000)
for (m in 1:10000){
  a <- sample(1:100,size=1,replace=T,prob=alphamarginal)
  b <- sample(1:100,size=1,replace=T,prob=betaconditional[a,])
  alpha.samp[m] <- alpharange[a]
  beta.samp[m] <- betarange[b]
}

par(mfrow=c(1,1))
contour(alpharange,betarange,full,xlab="alpha",ylab="beta",drawlabels=F,col=2)
points(alpha.samp,beta.samp)

## calculating posterior means/intervals for alpha and beta

par(mfrow=c(2,1))
hist(alpha.samp,main="Alpha Samples")
hist(beta.samp,main="Beta Samples")

mean(alpha.samp)
mean(beta.samp)

alpha.sampsort <- sort(alpha.samp)
beta.sampsort <- sort(beta.samp)

alpha.sampsort[250]
alpha.sampsort[9750]
beta.sampsort[250]
beta.sampsort[9750]

sum(beta.samp >= 0)/10000

par(mfrow=c(1,1))
plot(t,y,pch=19)
for (i in 1:1000){
  abline(alpha.samp[i],beta.samp[i],col=3)
}
points(t,y,pch=19)

## predicted new observation for 1986 (t = 10):

pred.rate <- alpha.samp + beta.samp*10

pred.accidents <- rep(NA,10000)
for (i in 1:10000){
  pred.accidents[i] <- rpois(1,pred.rate[i])
}

mean(pred.accidents)
sort(pred.accidents)[250]
sort(pred.accidents)[9750]

par(mfrow=c(2,1))
hist(pred.accidents,xlim=c(0,45))
hist(y,xlim=c(0,45))

