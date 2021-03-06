#############################################
########## READING IN BODYFAT DATA ##########
#############################################

data <- read.table("data/bodyfat (1).csv",header=T,sep=",")
attach(data)
data[1:5,]

par(mfrow=c(1,2))
plot(abdomen,weight,pch=19)
plot(wrist,weight,pch=19)

#############################################
####### MLE FIT OF REGRESSION MODEL #########
#############################################

model <- lm(weight~abdomen+wrist)
summary(model)
par(mfrow=c(1,1))
plot(model$fitted.values,model$residuals,pch=19)
abline(h=0,col=2)

#############################################
### SAMPLING FROM POSTERIOR DISTRIBUTION ####
#############################################


beta.hat <- model$coef
n <- length(weight)
p <- length(beta.hat)
s2 <- (n-p)*summary(model)$sigma^2
V.beta <- summary(model)$cov.unscaled

numsamp <- 1000
beta.samp <- matrix(NA,nrow=numsamp,ncol=p)
sigsq.samp <- rep(NA,numsamp)
for (i in 1:numsamp){
  #sample sigma and invert
	temp <- rgamma(1,shape=(n-p)/2,rate=s2/2)
	cursigsq <- 1/temp
	curvarbeta <- cursigsq*V.beta
	curvarbeta.chol <- t(chol(curvarbeta))
	#sample from the normal
	z <- rnorm(p,0,1)
	curbeta <- beta.hat+curvarbeta.chol%*%z
	sigsq.samp[i] <- cursigsq
	beta.samp[i,] <- curbeta
}
beta.samp

#############################################
#### SUMMARIZING POSTERIOR DISTRIBUTIONS ####
#############################################

## posterior means
postmean.beta <- apply(beta.samp,2,mean)
postmean.sigsq <- mean(sigsq.samp)
postmean.beta
postmean.sigsq

## posterior correlation between variables
cor(cbind(beta.samp,sigsq.samp))

## 95% posterior intervals
allsamples <- cbind(beta.samp,sigsq.samp)
allsamples.sort <- apply(allsamples,2,sort)
allsamples.sort[25,]
allsamples.sort[975,]

## posterior histograms
par(mfrow=c(2,2))
hist(allsamples[,1],main="Intercept")
abline(v=postmean.beta[1],col=2)
abline(v=beta.hat[1],col=3)
hist(allsamples[,2],main="Abdomen")
abline(v=postmean.beta[2],col=2)
abline(v=beta.hat[2],col=3)
hist(allsamples[,3],main="Wrist")
abline(v=postmean.beta[3],col=2)
abline(v=beta.hat[3],col=3)
hist(allsamples[,4],main="Sigsq")
abline(v=postmean.sigsq,col=2)
abline(v=summary(model)$sigma^2,col=3)

#############################################
#### POSTERIOR PREDICTIVE DISTRIBUTION ######
#### FOR A NEW COVARIATE VECTOR Xstar  ######
#############################################

Xstar <- c(1,130,17)  # new person with abdomen = 130 and wrist = 17
Xstar <- t(Xstar)  # making it a row vector

beta.samp
## use posterior samples from before:

ystar.samp <- rep(NA,numsamp)
for (i in 1:numsamp){
   xstarbeta <- Xstar%*%t(t(beta.samp[i,]))
   ystar.samp[i] <- rnorm(1,mean=xstarbeta,sd=sqrt(sigsq.samp[i]))
}   

ystar.postmean <- mean(ystar.samp)
ystar.postmean

par(mfrow=c(2,1))
xmin <- min(weight,ystar.postmean)
xmax <- max(weight,ystar.postmean)
hist(weight,main="Dataset Weights",xlim=c(xmin,xmax))
abline(v=mean(weight),col=2)
hist(ystar.samp,main="Predicted Weight of Person with abdomen = 130 and wrist = 17",xlim=c(xmin,xmax))
abline(v=ystar.postmean,col=2)
