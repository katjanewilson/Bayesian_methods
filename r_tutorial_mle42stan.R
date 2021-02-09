##install packages

install.packages('devtools')
install.packages("remotes")
remotes::install_github("rmcelreath/glmer2stan")
devtools::install_github('rmcelreath/glmer2stan')
usethis::browse_github_pat()
usethis::edit_r_environ()
usethis::edit_r_environ(token = 59336fb94563b0b475e5bb26b57c6641632a4178)
source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
install.packages("rstan")
source_url("https://github.com/stan-dev/shinystan/raw/develop/install_shinystan.R")
install_shinystan()
install.packages('lmerTest')
install.packages("glmer2stan")


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



################ glmer2stan
##step 4
################


data("sleepstudy")
#days nested within subjects
#so you can look at both random slopes and intercepts
data(sleepstudy)


# plot effects first!

# no random effects model
m1_lm <- lm(Reaction~Days,data=sleepstudy)
confint(m1_lm)
summary(m1_lm)

ggplot(sleepstudy,aes(x=Days,y=Reaction))+
  geom_point()+
  guides(color=F)+
  geom_smooth(method=lm,se = F)


# with nesting of intercepts and slopes within subjs
ggplot(sleepstudy,aes(x=Days,y=Reaction,color=Subject,group=Subject))+
  geom_point()+
  guides(color=F)+
  geom_smooth(method=lm,se = F)

# model in regular ol' lme4, random slopes and intercepts set
m1_lme4 <- lmer( Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE )
summary(m1_lme4)
confint(m1_lme4)

AIC(m1_lm,m1_lme4) # fixed effects the same, model is more parsimonious i.e. random effects = good



# convert factor of subject ids to sequential integers
sleepstudy$subject_index <- as.integer(as.factor(sleepstudy$Subject)) 

# basic MCMC parameter, should probably be a bit higher but we dont have all day!
nwarm = 100 # burn-in period, these samples are not included in estimation
niter = 500 # number of steps per chain, more is better (but takes longer)
chains = 4 # number of chains, usually at least 2

m1_g2s <- lmer2stan( Reaction ~ Days + (Days | subject_index), data=sleepstudy,
                     calcWAIC=T,
                     warmup=nwarm, 
                     iter = niter, 
                     chains=chains) 




