##hiv example

#method 1
prior.1st = c(0.00148, 1-0.00148)
prior.1st
likelihood = c(0.93, 1-0.99)
posterior.1st = prior.1st * likelihood / sum(prior.1st * likelihood)

prior.2nd = posterior.1st
posterior.2nd = prior.2nd * likelihood / sum(prior.2nd * likelihood)
posterior.2nd[1]

#method 2
prior = c(0.00148, 1-0.00148)
likelihood = c(dbinom(2, size = 2, prob = 0.93),
               dbinom(2,size = 2, prob = 1-0.99))
posterior = prior *likelihood / sum(prior *likelihood)
posterior[1]

##frequentist vs. bayesian approach

#frequentist
pbinom(0, size = 5, prob = 0.1, lower.tail = FALSE)
n = c(10,15,20)
k = c(2,3,4)
probs = mapply(function(k,n) pbinom(k-1, size = n, prob = 0.1,
                                    lower.tail = FALSE), k, n)
round(probs, 2)

#bayesian
k = c(2,3,4)
n = c(10,15,20)

#calculate posterior probability of each hypothesis
prior = c(0.5, 0.5)
likelihood = mapply(function(k,n) dbinom(k, size = n, 
                                         prob = c(0.1,0.2)),
                    k,n)
likelihood = t(likelihood)
likelihood
posterior = prior * likelihood /rowSums(prior * likelihood)

#print
colnames(posterior) = c("H1", "H2")
rownames(posterior) = c("n=10", "n=15", "n=20")
round(posterior, 2)
