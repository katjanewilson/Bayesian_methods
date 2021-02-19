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
