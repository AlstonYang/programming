#John's payoff is deterministic
John_pay = 12000

#The probability of getting the job from Vanessa
prob_interval = seq(0.1,0.9,0.1)

#Simulation runs
simulation_runs =10000


#Earnings of waiting for Vanessa's uncertain offer
results1[0.1]  = matrix(NA, nrow = length(prob_interval), ncol = simulation_runs)
rownames(results1) = prob_interval
colnames(results1) = 1:simulation_runs

results1[1,]
sim.School_pay = sample(c(21600,16800,12000,6000,0),S,prob = c(0.05,0.25,0.4,0.25,0.05),replace = TRUE)

for(p in prob_interval){
  for(i in 1:length(simulation_runs)){
    
    #The payoff from Vanessa is deterministic
    Vanessa_pay = 14000
    #The chance to get the job from Vanessa
    Vanessa_offer = sample(c(1,0), 1, prob = c(p,1-p))
    
    Vanessa_pay = Vanessa_pay * Vanessa_offer
    
    #The payoff from school is uncertain
    School_pay = sim.School_pay[i]
    
    if(Vanessa_offer==1){
      results1[which(p==prob_interval),i]=Vanessa_pay
    }else{
      results1[which(p==prob_interval),i]=School_pay
    }
  }
  print(results1[1,])
}



#Earnings of waiting for Vanessa's uncertain offer
pctile.range = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
sim.pctile1 = matrix(NA, nrow = length(prob_interval), ncol=length(pctile.range))
rownames(sim.pctile1)=prob_interval
colnames(sim.pctile1)=pctile.range

#apply(results1,1,summary)

for(i in 1:length(prob_interval)){
  for (j in 1:length(pctile.range)) {
    sim.pctile1[i,j] = quantile(results1[i,],pctile.range [j]) 
  }
}

sim.pctile1


# the number of times EMV > john's pay
Pbetter1=c()
for(i in 1:nrow(results1)){
  Pbetter1[i] = sum(results1[i,]>=John_pay)/ncol(results1)
}

x11(width=8,height=5)
plot(prob_interval,Pbetter1,type='l',xaxt='n',lwd=3,
     xlab="P(Vanessa offer=1)",ylab="P(Earning>=John offer)")
axis(1,seq(0.1,0.9,0.1))
abline(h=0.95,col='red',lty=2,lwd=2)
abline(h=0.9,col='green',lty=3,lwd=2)
abline(h=0.85,col='blue',lty=4,lwd=2)

