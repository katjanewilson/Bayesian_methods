### Part 1: read in data
data <- read.table("data/batting.1975-2016.csv",header=T,sep=",")
data <- data[data$AB >=100,]
dim(data)
data <- data[data$HR >=1, ]
HR <- data$HR
AB <- data$AB
player <- data$player
year <- data$year
HR.AVG <- HR/AB


#Part 2: Expectation function
Estep2 <- function(y,alpha,mu1,sigsq){
  n <- length(y)  
  ind <- rep(NA,n)
  ##prob0 <- (1-alpha)*dnorm(y[i],mean=mu0,sd=sqrt(sigsq))
  prob0 <- alpha*dnorm(y,mean=mu1,sd=sqrt(sigsq))
  prob1 <- 2*(1-alpha)*dnorm(y,mean=0,sd=sqrt(sigsq))
  ind <- prob1/(prob0+prob1)
  ind
}

# log likelihood
loglik.mix <- function(y,ind,alpha,mu1,sigsq){
  loglik <- sum(log(alpha*dnorm(y,mu1,sqrt(sigsq))+2*(1-alpha)*dnorm(y,mean=0,sd=sqrt(sigsq))))
  loglik
}

#Part 3: Maximization function
Mstep2 <- function(y,ind){
  n <- length(y)
  alpha <- sum(ind)/n
  mu1 <- sum(ind*y)/sum(ind)
  ## get rid of the mu0
  sigsq <- sum(ind*((y-mu1)^2))
  ## make this y
  sigsq <- sigsq+sum((1-ind)*((y-0)^2))
  sigsq <- sigsq/n
  c(alpha,mu1,sigsq)
}


#Part 4: Running EM iterations

curalpha <- 0.6
curmu1 <- 0.001
cursigsq <- 1
curind <- Estep2(HR.AVG,curalpha,curmu1,cursigsq)
loglik <- loglik.mix(HR.AVG,curind, curalpha, curmu1, cursigsq)
itermat2 <- c(curalpha,curmu1,cursigsq,loglik)

diff <- 1
numiters <- 1
while (diff > 0.001 || numiters <= 100){
  curind <- Estep2(HR.AVG,curalpha,curmu1,cursigsq)
  curparam <- Mstep2(HR.AVG,curind)
  curalpha <- curparam[1]
  curmu1 <- curparam[2]
  cursigsq <- curparam[3]
  loglik <- loglik.mix(HR.AVG,curind,curalpha,curmu1,cursigsq)
  itermat2 <- rbind(itermat2,c(curparam,loglik))
  numiters <- numiters + 1
  diff <- max(abs(itermat2[numiters,]-itermat2[numiters-1,])) 
  print (c(numiters,loglik))
}


#Tracking iterations
parametertext <- c("alpha","mu0","mu1","sigsq","loglik")
par(mfrow=c(2,3))
for (i in 1:5){
  plot(1:numiters,itermat2[,i],type="l",main=parametertext[i],xlab="Iterations",ylab="Value")
}

# Part 7
lastiter <- length(itermat2[,1])
EM.alpha.1 <- itermat2[lastiter,][1]
Em.mu1.1 <- itermat2[lastiter,][2]
Em.sigsq1.1 <- itermat2[lastiter,][3]
EM.alpha.beta.1 <- itermat2[lastiter,][4]
EM.beta.1 <- itermat2[lastiter,][5]
results_MLE_EM <- matrix(NA, 6,1)
results_MLE_EM[ , 1] <- itermat[lastiter,]
rownames(results_MLE_EM) <- c("Alpha", "Mean", "Variance", "alpha of the beta", "beta", "lik")
colnames(results_MLE_EM) <- "MLE Values"

#Part 8: Plotting
finalparam<-itermat2[numiters,]
alpha <- finalparam[1]
mu1 <- finalparam[2]
sigsq <- finalparam[3]
par(mfrow=c(1,1))
hist(HR.AVG,prob=T)
x <- ppoints(1000)*0.15
# y1 <- (1-alpha)*dnorm(x,mu0,sqrt(sigsq0))
# y2 <- alpha*dnorm(x,mu1,sqrt(sigsq1))
y1 <- (1-alpha)*dnorm(x,mu1, sqrt(sigsq))
y2 <- 2*alpha*dnorm(x,0, sqrt(sigsq))
lines(x,y1,col=2)
lines(x,y2,col=3)

##Individuals

finalindprops <- Estep2(HR.AVG,alpha,mu1,sigsq)

hist(finalindprops)
sum(finalindprops > 0.9999)
players.topHR<-data[finalindprops > 0.9999,1:5]
players.topHR

##ortiz
### ortiz
y <- HR.AVG
id <- data$playerID
id <- id[index]
player_index <- id == "ortizda01"
ind <- curind
Estep2(y[player_index], alpha, mu1, sigsq)


# data["Average Home Run"] <- HR.AVG
# data.ortiz <- data[data$playerID == "ortizda01", ]
# years.ortiz <- data.ortiz$yearId
# prob.ortiz <- Estep2(y=data.ortiz$"Average Home Run", alpha, mu1, sigsq)
# prob.ortiz




data["Average Home Run"] <- HR.AVG
data.ortiz <- data[data$playerID == "ortizda01", ]
years.ortiz <- data.ortiz$yearId
prob.ortiz <- Estep2(y=data.ortiz$"Average Home Run",alpha,mu1,sigsq)
prob.ortiz
ortiz.stints.everyseason <- matrix(NA, length(data.ortiz$playerID), 2)
ortiz.stints.everyseason[,1] <- data.ortiz$yearID
ortiz.stints.everyseason[,2] <- prob.ortiz
colnames(ortiz.stints.everyseason) = c("season", "Probability of vbeing elite")
ortiz.stints.everyseason.1 <- ortiz.stints.everyseason[1:10, ]
ortiz.stints.everyseason.2 <- ortiz.stints.everyseason[11:18, ]
plot(ortiz.stints.everyseason[,1], prob.ortiz, col = "red", xlab = "season")

