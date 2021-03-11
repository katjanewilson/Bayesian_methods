
data <- read.table("data/batting.1975-2016.csv",header=T,sep=",")
homerun <- data$HR
bat <- data$AB

index <- homerun >0 & bat > 99.9
homerun <- homerun[index]
bat <- bat[index]
average_run <- homerun/bat

## make a grid of alpha and beta values
alpha <- seq(2.65, 2.9, length.out = 100)
beta <- seq(85,91, length.out = 100)

log_lik_compute <- matrix(NA, nrow = 100, ncol = 100)

for( i in 1:100){
  print(i)
  for(j in 1:100){
    log_lik_compute[i,j] <- sum(dbeta(average_run,
                                      alpha[i],
                                      beta[j],
                                      log = T))
  }
}

full <- log_lik_compute
full <- exp(full - max(full))
full <- full/sum(full)
contour(alpha, beta, full, xlab = "alpha", ylab = "beta", drawlabels = F)


### question 3

max(log_lik_compute)
which(log_lik_compute == max(log_lik_compute), arr.ind = T)
alpha_mle <- alpha[46]
beta_mle <- beta[51]

#MLE estimates variance
nr <- (alpha_mle*beta_mle)
dr1 <- (alpha_mle + beta_mle)^2
dr2 <- (alpha_mle + beta_mle +1)
var_mle <- nr/(dr1*dr2)

#actual data variance
var(average_run)


### question 4

loglik_beta <- function(start_val, x){
  sum(-dbeta(x,
             start_val[1],
             start_val[2],
             log = TRUE))
}

out <- optim(par = c(0.1,0.1),
             fn = loglik_beta,
             x = average_run,
             method = "Nelder-Mead")
out$par
