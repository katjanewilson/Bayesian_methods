### Part 1: read in data
data <- read.table("data/batting.1975-2016.csv",header=T,sep=",")
data <- data[data$AB >=100,]
dim(data)
data <- data[data$HR >=1, ]
HR <- data$HR
AB <- data$AB
player <- data$player
year <- data$year
HR.average <- HR/AB

y<- HR.average
n <- length(HR.average)
mu.data <- mean(HR.average)
sd.data <- sd(HR.average)
var.data <-sd.data^2
alpha.data <- mu.data*(mu.data*(1-mu.data)/var.data - 1)
beta.data <- (1-mu.data)*(mu.data*(1-mu.data)/var.data -1)

#Part 2: Expectation function
Estep <- function(y,alpha, mu1, sigsq1, alpha.beta, beta){
  n <- length(y)  
  ind <- rep(NA,n)
  for(i in 1:n) {
    prob0 <- (1-alpha)*dbeta(y[i], alpha.beta, beta)
    prob1 <- alpha*dnorm(y[i],mean=mu1,sd=sqrt(sigsq1))
    ind[i] <- prob1/(prob0+prob1)
  }
  ind
}


#negative log likelihood
negative.log.likelihood <- function(theta, y, alpha, mu1, sigsq1, ind){
  n <- length(y)
  alpha.beta <- theta[1]
  beta <- theta[2]
  total <- sum(log((alpha*dnorm(y,mu1, sqrt(sigsq1))) + ((1-alpha)*dbeta(y,alpha.beta, beta))))
  return(-total)
}

#Part 3: Maximization function
Mstep <- function(y,ind, alpha.initial, beta.initial){
  n <- length(y)
  alpha <- sum(ind)/n
  mu1 <- sum(ind*y)/sum(ind)
  sigsq1 <- sum(ind*((y-mu1)^2))/sum(ind)
  results.optim <- optim(par = c(alpha.initial, beta.initial),
                         fn = negative.log.likelihood,
                         y = y, alpha = alpha,
                         mu1 = mu1, sigsq1 = sigsq1)
  c(alpha,mu1,sigsq1, results.optim$par[1], results.optim$par[2])
}


#Part 4: Running EM iterations

curalpha <- 0.05
curmu1 <- 0.02
cursigsq1 <- 0.03
alpha.initial <- alpha.data
beta.initial <- beta.data
itermat <- c(curalpha, curmu1, cursigsq1, alpha.initial, beta.initial)

diff <- 1
numiters <- 1
while (diff > 0.00001 || numiters <= 100){
  numiters <- numiters + 1
  curind <- Estep(HR.average,curalpha,curmu1, cursigsq1, alpha.beta = alpha.initial,
                  beta = beta.initial)
  curparam <- Mstep(HR.average,curind, alpha.initial, beta.initial)
  curalpha <- curparam[1]
  curmu1 <- curparam[2]
  cursigsq1 <- curparam[3]
  alpha.initial <- curparam[4]
  beta.initial <- curparam[5]
  loglik <- -negative.log.likelihood(c(alpha.initial, beta.initial), y, alpha = curalpha,
                                     mu1 = curmu1, sigsq1 = cursigsq1)
  itermat <- rbind(itermat, c(curparam, loglik))
  diff <- max(abs(itermat[numiters,]-itermat[numiters-1,])) 
  print (c(numiters,loglik))
}

#Tracking iterations
parametertext <- c("alpha", "mu1", "sigsq1", "alpha",
                   "beta", "log likelihood")
par(mfrow = c(2,3))
for (i in 1:6){
  plot(1:length(itermat[,1]),itermat[,i],main=parametertext[i],xlab="Iterations",ylab="Value")
}


# Part 7
lastiter <- length(itermat[,1])
EM.alpha.1 <- itermat[lastiter,][1]
Em.mu1.1 <- itermat[lastiter,][2]
Em.sigsq1.1 <- itermat[lastiter,][3]
EM.alpha.beta.1 <- itermat[lastiter,][4]
EM.beta.1 <- itermat[lastiter,][5]
results_MLE_EM <- matrix(NA, 6,1)
results_MLE_EM[ , 1] <- itermat[lastiter,]
rownames(results_MLE_EM) <- c("Alpha", "Mean", "Variance", "alpha of the beta", "beta", "lik")
colnames(results_MLE_EM) <- "MLE Values"

#Part 8: Plotting
par(mfrow = c(1,1))
hist(y, prob = T, main = "Histogram of Home Run Averages", ylim = c(0,25), xlab = "Home Run Average")
x <- ppoints(1000)*0.15
y1 <- EM.alpha.1*dnorm(x, mean = Em.mu1.1, sd = sqrt(Em.sigsq1.1))
y2 <- (1-EM.alpha.1)*dbeta(x,EM.alpha.beta.1, EM.beta.1)
lines(x,y1, col =2)
lines(x, y2, col =3)
legend(0.07, 20, col = c("red", "green"), lty = 1:1, cex = 1)



##Individuals

finalindprops <- Estep(HR.average,alpha,mu1,sigsq)
hist(finalindprops)
sum(finalindprops > 0.9999)
players.topHR<-data[finalindprops > 0.9999,1:5]
players.topHR

##ortiz
y <- HR.average
id <- data$playerID
player_index <- id == "ortizda01"
ind <- curind
Estep(y[player_index], EM.alpha.1,
      Em.mu1.1, Em.sigsq1.1,
      EM.alpha.beta.1, EM.beta.1)


