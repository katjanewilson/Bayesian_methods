###################################################################
#### Normal Model with Conjugate and Noninformative Priors      ###
###################################################################

## Reading in Data:
data <- read.table("data/hitters.post1970.txt",header=T,row.names=NULL)
dim(data)

data[1:5,]

## Reducing data to player-seasons where ab >= 200
data <- data[data$AB >= 200,]
dim(data)


## Calculating batting average
ba <- data$H/data$AB
n <- length(ba)
hist(ba,main="Histogram of Batting Average")
min(ba)
data[which(ba==min(ba)),]
max(ba)
data[which(ba==max(ba)),]


sample.norm.conj <- function(y,mu0,kappa0,nu0,sigsq0,numsamp){
    n <- length(y)
	y.mean <- mean(y)
	y.ss <- var(y)*(n-1)
	discrep <- (kappa0*n/(kappa0+n))*(y.mean-mu0)^2
	x <- rgamma(numsamp,shape=(nu0+n)/2,rate=(nu0*sigsq0+y.ss+discrep)/2)
    sigsq.samp <- 1/x
	postvar <- 1/(n/sigsq.samp + kappa0/sigsq.samp)
	postmean <- (n*y.mean/sigsq.samp + kappa0*mu0/sigsq.samp)/(n/sigsq.samp + kappa0/sigsq.samp)
    mu.samp <- rnorm(numsamp,mean=postmean,sd=sqrt(postvar))
    out <- cbind(mu.samp,sigsq.samp)
    out
}

## checking the posterior for different conjugate priors and non-informative 
##  (different values of mu0,kappa0)

theta1 <- sample.norm.conj(ba,0.2,10,10,10,1000)   # mu0 = 0.2, kappa0 = 10, etc.
theta2 <- sample.norm.conj(ba,0.2,100,10,10,1000)   # mu0 = 0.2, kappa0 = 100, etc.
theta3 <- sample.norm.conj(ba,0.2,1000,10,10,1000)   # mu0 = 0.2, kappa0 = 1000, etc.
theta4 <- sample.norm.conj(ba,0.2,0,0,10,1000)   # non-informative kappa0 = 0, nu0 = 0 

par(mfrow=c(2,2))
minmu <- min(theta1[,1],theta2[,1],theta3[,1],theta4[,1],mean(ba))
maxmu <- max(theta1[,1],theta2[,1],theta3[,1],theta4[,1],mean(ba))
hist(theta3[,1],main="Mu: mu0=0.2,kappa0=1000",xlim=c(minmu,maxmu),xlab="mu")
abline(v=mean(ba),col=2)
hist(theta2[,1],main="Mu: mu0=0.2,kappa0=100",xlim=c(minmu,maxmu),xlab="mu")
abline(v=mean(ba),col=2)
hist(theta1[,1],main="Mu: mu0=0.2,kappa0=10",xlim=c(minmu,maxmu),xlab="mu")
abline(v=mean(ba),col=2)
hist(theta4[,1],main="Mu: non-informative",xlim=c(minmu,maxmu),xlab="mu")
abline(v=mean(ba),col=2)

## could have also generated mu directly from t distribution:
mu.sampt <- rt(10000,n-1)
mu.sampt <- mu.sampt*sqrt(var(ba)/n)+mean(ba)

## compare to original sampling scheme
theta <- sample.norm.conj(ba,0.2,0,0,10,10000)
mu.orig <- theta[,1]
    	
xmin <- min(mu.orig,mu.sampt)
xmax <- max(mu.orig,mu.sampt)
par(mfrow=c(2,1))
hist(mu.orig,main="Mu: non-informative",xlim=c(xmin,xmax))
abline(v=mean(ba),col=2)
hist(mu.sampt,main="Mu: from t dist",xlim=c(xmin,xmax))
abline(v=mean(ba),col=2)

###################################################################
#### Grid Search and Grid Sample: Semi-Conjugate Normal Example ###
###################################################################

## Reading in Data:
data <- read.table("data/hitters.post1970.txt",header=T)

## Reducing data to player-seasons where ab > 200
data <- data[data$AB > 200,]


## Calculating batting average
ba <- data$H/data$AB
n <- length(ba)
hist(ba)
min(ba)
data[which(ba==min(ba)),]
max(ba)
data[which(ba==max(ba)),]


evaluatepostsigsq <- function(sigsqvalues,y,mu0,nu0,tausq0,sigsq0){
	m <- length(sigsqvalues)
	logvals <- rep(0,m)
	n <- length(y)
	for (i in 1:m){
		cursigsq <- sigsqvalues[i]
		postmean  <- (n*mean(y)/cursigsq + mu0/tausq0)/(n/cursigsq + 1/tausq0)
		for (j in 1:n){
			logvals[i] <- logvals[i] + dnorm(y[j],mean=postmean,sd=sqrt(cursigsq),log=T)
		}
		logvals[i] <- logvals[i] - 0.5*log(n/cursigsq + 1/tausq0)
		logvals[i] <- logvals[i] - (0.5*nu0+1)*log(cursigsq) - nu0*sigsq0/(2*cursigsq)
		print (i)
	}
	out <- exp(logvals-max(logvals))
	out
}

## setting hyperparameter values
tausq0 <- 0.1
sigsq0 <- 0.1
nu0 <- 0.001
mu0 <- 0.2

sigsqgrid <- ppoints(100)
sigsqprobs <- evaluatepostsigsq(sigsqgrid,ba,mu0,nu0,tausq0,sigsq0)
par(mfrow=c(1,1))
plot(sigsqgrid,sigsqprobs,type="l",main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)")

sigsqgrid <- ppoints(100)*0.01
sigsqprobs <- evaluatepostsigsq(sigsqgrid,ba,mu0,nu0,tausq0,sigsq0)
par(mfrow=c(1,1))
plot(sigsqgrid,sigsqprobs,type="l",main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)")


sigsqgrid <- ppoints(100)*0.002
sigsqprobs <- evaluatepostsigsq(sigsqgrid,ba,mu0,nu0,tausq0,sigsq0)
par(mfrow=c(1,1))
plot(sigsqgrid,sigsqprobs,type="l",main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)")

sigsqgrid <- ppoints(100)*0.00015+0.000875
sigsqprobs <- evaluatepostsigsq(sigsqgrid,ba,mu0,nu0,tausq0,sigsq0)
par(mfrow=c(1,1))
plot(sigsqgrid,sigsqprobs,type="l",main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)")

## optimal point estimate approximated by grid point with highest value
sigsqhat <- sigsqgrid[which(sigsqprobs==max(sigsqprobs))]
abline(v=sigsqhat,col=2)


## grid sampling: sample 1000 values of sigsq proportional to sigsqprobs

sigsqprobs <- sigsqprobs/sum(sigsqprobs)
sigsq.samp <- sample(sigsqgrid,size=1000,replace=T,prob=sigsqprobs)

plot(sigsqgrid,sigsqprobs,type="l",main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)",xlim=c(0.000875,0.001025),col=2,lwd=2)
par(new=T)
hist(sigsq.samp,prob=T,xlim=c(0.000875,0.001025),main="Posterior Dist. of Sigsq (Semi-Conjugate Prior)",xlab="",ylab="",yaxt="n",breaks=25,col=)

## sampling mu given sampled sigmasq

mu.samp.semiconjugate <- rep(NA,1000)
for (i in 1:1000){
	postvar <- 1/(n/sigsq.samp[i] + 1/tausq0)
	postmean <- (n*mean(ba)/sigsq.samp[i] + mu0/tausq0)*postvar
	mu.samp.semiconjugate[i] <- rnorm(1,mean=postmean,sd=sqrt(postvar))
}
hist(mu.samp.semiconjugate,main="Post.Dist. of Mu (Semi-Conjugate Prior)")
abline(v=mean(ba),col=2)


############################################################
### Grid Search and Grid Sample: Poisson Planes Dataset ####
############################################################

## input data:  
data <- read.table("planes.txt",skip=1)
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


