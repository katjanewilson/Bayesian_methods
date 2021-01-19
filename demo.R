##install packages

install.packages('devtools')
install_github('rmcelreath/glmer2stan')
source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install.packages("rstan")
source_url("https://github.com/stan-dev/shinystan/raw/develop/install_shinystan.R")
install_shinystan()
install.packages('lmerTest')


library(devtools)
library(shinyStan)
library(glmer2stan)
library(rstan)
library(lmerTest)

################ simulate data
##step 1
################

height = rnorm(100)
earn = height + rnorm(100)

dat = data.frame(height,earn)

################ frequentist way
##step 2
################

library(qqplot2)
qplot(height, earn) + geom_smooth(method = lm, se=F)

#when height is 0, earn is around 0
#slope- for every 1 unit increase in height, you
#get 1 unit increase in earn
summary(lm(earn~height, data = dat))
confint(lm(earn~height, data = dat))

################ hard code stan method
##step 3
################
earn_dat <- list(N = 100, #specify nubmer of observations in scalar
                 earn = earn,
                 height- height
                 )

#what the stan code looks like
earn_code = 'data {
  // First we declare all of our variables in the data block
  int<lower=0> N;// Number of observations
  vector[N] earn; //Identify our predictor as a vector
  vector[N] height;  //Identify our outcome variable as a vector
}
parameters {
  vector[2] beta; //Our betas are a vector of length 2 (intercept and slope)
  real<lower=0> sigma; //error parameter
}
model {
  //Priors
  beta[1] ~ normal( 0 , 100); //intercept
  beta[2] ~ normal( 0 , 100 ); //slope
  sigma ~ uniform( 0 , 100 ); //error
  earn ~ normal(beta[1] + beta[2] * height, sigma);
}'


fit1 <- stan(model_code = earn_code, data = earn_dat,
             warmup = 100,
             iter = 1000, 
             chains = 4)
print(fit1)



################ lme4_to_stan
##step 4
################


