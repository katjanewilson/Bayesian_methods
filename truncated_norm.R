### modelling distributions

## first, the normal distribution

sigsq1 = 1
dnorm(y[i],mean=mu1,sd=sqrt(sigsq1))
library(ggplot2)
u <- runif(10000)
x <- qnorm(u, mean = 0, sd = 3)
ggplot(data = data.frame(x), aes(x = x)) +
  geom_histogram(fill = "#CCC591", alpha = 1, binwidth = .2, boundary = 0) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())
library(truncnorm)
rtruncnorm(100, a=-Inf, b=5, mean=3, sd=2) 

## example
u <- runif(10000)
x <- rtruncnorm(u, a = -Inf, b = 5, mean = 0, sd = 3)
ggplot(data = data.frame(x), aes(x = x)) +
  geom_histogram(fill = "#CCC591", alpha = 1, binwidth = .2, boundary = 0) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())


## example
u <- runif(10000)
x <- rtruncnorm(u, a = 0, b = Inf, mean = 0, sd = 3)
?dtruncnorm
x<- dtruncnorm(u, a = 0, b = Inf, mean = 0, sd = 3)
ggplot(data = data.frame(x), aes(x = x)) +
  geom_histogram(fill = "#CCC591", alpha = 1, binwidth = .2, boundary = 0) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())
