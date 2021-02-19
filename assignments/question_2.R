##### question 2
set.seed(2220)
sim.num.patients <- c()
sim.num.waiting <- c()
sim.office.close.mings <- c()
med.sim.office.close.mins <- c()
sim.office.close.time <- c()

j<- 0

while(j < 100) {
  arrival.times <- c(0)
  num.patients <- length(arrival.times)
  next.patient <- 0
  
while(arrival.times[length(arrival.times)] < 420){
    next.patient <- next.patient +rexp(1,.1)
    arrival.times <- c(arrival.times,next.patient)
    arrival.times[length(arrival.times)]
    num.patients <- length(arrival.times)
}
arrival.times <- arrival.times[c(-1, -length(arrival.times))]
arrival.times
num.patients <- length(arrival.times)
cat('Number of patients who came to the office: ', num.patients, 'patients\n')

arrival.times <- arrival.times[c(-1, -length(arrival.times))]
num.patients <- length(arrival.times)
sim.num.patients <- c(sim.num.patients, num.patients)
  
doc.avail <-c(0,0,0)
d <-0
time.with.doc <- runif(num.patients, 5, 20)
i <- 0
total.waiting.time <- c()

for (i in c(1:num.patients)){
  d <- which.min(doc.avail)
  doc.avail[d] <- max(doc.avail[d], arrival.times[i])+time.with.doc[i]
  ind.waiting.time <-max(doc.avail[d], arrival.times[i]) - arrival.times[i]
  total.waiting.time <- c(total.waiting.time, round(ind.waiting.time-time.with.doc[i] ,3))
  only.waiting.time <- total.waiting.time[total.waiting.time >0]
}

j = j+1
print(sim.num.patients)

}
sim.num.patients
length(sim.num.patients)
num.waiting <- length(only.waiting.time)
num.waiting
mean(ind.waiting.time)


sim_1000_number_patients <- rpois(1000,sim.num.patients)
sim_1000_number_patients
print(sort(sim_1000_number_patients)[c(25,976)])
sim_1000_num_waiting<- rpois(1000,num.waiting)
sim_1000_num_waiting
print(sort(sim_1000_num_waiting)[c(25,976)])
sim_1000_waiting_times <- rpois(1000,only.waiting.time)
sim_1000_waiting_times 
print(sort(sim_1000_waiting_times )[c(25,976)])
