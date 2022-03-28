##The optimal stopping problem
set.seed(5566)
p.candidate = seq(0.05,0.95,0.01)
S= 10000
population=100

score.matrix = matrix(NA, nrow = length(p.candidate), ncol = S)

for (s in 1:S){
  score = rnorm(population, 0, 1)
  optimal.maximum = max(score)
  
  for (p in 1:length(p.candidate)){
    max = max(score[1:(population*p.candidate[p])])
    
    for (i in (population*p.candidate[p]+1):population){
      score.i = score[i]
      
      if (score.i >= max){
        max = score.i
        break
      }
      
      if (i>=population){
        max = score.i
      }
    }
    score.matrix[p,s] = (max == optimal.maximum)
  }
}

score.avg = c()
for (p in 1:length(p.candidate)){
  score.avg[p] = sum(score.matrix[p,])/S
}

p.candidate[which.max(score.avg)]
max(score.avg)
plot(score.avg ~ p.candidate)


###---------------------------------------------
##A model for "Average Score"
p.candidate = seq(0.05,0.95,0.01)
population=1000

score.matrix = matrix(NA, nrow = length(p.candidate), ncol = S)

for (s in 1:S){
  score = rnorm(population, 0, 1)
  
  for (p in 1:length(p.candidate)){
    max = max(score[1:(population*p.candidate[p])])
    
    for (i in (population*p.candidate[p]+1):population){
      score.i = score[i]
      
      if (score.i >= max){
        max = score.i
        score.matrix[p,s] = max
        break
      }
      
      if (i>=population){
        score.matrix[p,s] = score.i
      }
    }
  }
}

score.avg = c()
for (p in 1:length(p.candidate)){
  score.avg[p] = mean(score.matrix[p,])
}

plot(score.avg ~ p.candidate)
p.candidate[which.max(score.avg)]
max(score.avg)

###---------------------------------------------
##The time of issue policy
#Assuming x1 and x2 are independent
set.seed(5566)
S = 10000

x1 = rnorm(S, 150, 30)
x2 = rnorm(S, 75, 25)
x = x1+x2

sum(x<=180)/S
quantile(x, 0.95)


issue.policy = function(){
  x1 = rnorm(3, 150, 30)
  x2 = rnorm(3, 75, 25)
  
  t12 = x1[1]+x1[2]
  t21 = x1[1]+x2[1]
  
  t13 = sum(x1)
  t22 = max(t12,t21)+x2[2]
  
  t23 = max(t13, t22) + x2[3]
  return(t23)
}

issue.time = c()
for(i in 1:S){
  issue.time [i] = issue.policy()
}



summary(issue.time)
sum(issue.time<=480)/S

###---------------------------------------------
##Covariance and correlation
p = c(0.1, 0.15, 0.05, 0.1, 0.2, 0.05, 0.1, 0.1, 0.1, 0.05)
x1 = c(35, 78, 81, 30, 16, 29, 35, 14, 52, 46)  
x2 = c(41, 10, 0, 13, 42, 22, 1, 26, 11, 23)
mean.x1 = sum(p*x1)
mean.x2 = sum(p*x2)

sd.x1 = sqrt(sum(p*(x1 - mean.x1)^2))
sd.x2 = sqrt(sum(p*(x2 - mean.x2)^2))
sum(p*(x1 - mean.x1)*(x2-mean.x2))/(sd.x1*sd.x2)

cov(x1, x2)
cor(x1, x2)


###---------------------------------------------
##The time of issue policy
#Assuming x1 and x2 are dependent

mu.x1 = 150
sig.x1 = 30

mu.x2 = 75
sig.x2 = 25

corr = 0.37

#(a)
corr * sig.x1 * sig.x2

#(b)
##二元常態分佈

#(c)(d)(e)
set.seed(5566)
library(MASS)
corrXY=0.37
varcovMatrix=matrix(c(sig.x1^2, (corrXY*sig.x1*sig.x2), (corrXY*sig.x1*sig.x2), sig.x2^2), nrow = 2, ncol = 2)
Time1.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
Time2.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
Time3.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
#Random processing time of cases 1-3 for underwriting
Time11.corr=Time1.corr[,1]
Time12.corr=Time2.corr[,1]
Time13.corr=Time3.corr[,1]
#Random processing time of cases 1-3 for rating
Time21.corr=Time1.corr[,2]
Time22.corr=Time2.corr[,2]
Time23.corr=Time3.corr[,2]

BeginTime22.corr=c()
for(s in 1:S){
  BeginTime22.corr[s]=max(Time11.corr[s]+Time21.corr[s],
                          Time11.corr[s]+Time12.corr[s])
}

#Beginning time for the 3rd case of rating
BeginTime23.corr=c()
for(s in 1:S){
  BeginTime23.corr[s]=max(Time11.corr[s]+Time12.corr[s]+Time13.corr[s],
                          BeginTime22.corr[s]+Time22.corr[s])
}

#Ending time for the 3rd case of rating
EndTime.corr=BeginTime23.corr+Time23.corr

summary(EndTime.corr)

FinishTime1.corr=Time11.corr+Time21.corr
sum(FinishTime1.corr<=180)/S

quantile(FinishTime1.corr,0.95)

sum(EndTime.corr<=480)/S

###--------------------------------------------------------------
##Estimating correlations/covariances from data
Fund1=c(65, 79, 85, 78, 107, 108, 124, 156, 195, 181, 216)
Fund2=c(47, 61, 73, 60, 89, 86, 104, 120, 140, 134, 175)
Fund3=c(38, 37, 39, 40, 47, 46, 57, 71, 74, 72, 87)
Fund4=c(61, 64, 74, 72, 95, 89, 114, 147, 146, 127, 152)

AnnualGR=matrix(0,nrow=(length(Fund1)-1),ncol=4)


for(i in 2:length(Fund1)){
  AnnualGR[i-1,1]=Fund1[i]/Fund1[i-1]
  AnnualGR[i-1,2]=Fund2[i]/Fund2[i-1]
  AnnualGR[i-1,3]=Fund3[i]/Fund3[i-1]
  AnnualGR[i-1,4]=Fund4[i]/Fund4[i-1]
}


Sigma.est=cov(AnnualGR)
# cov2cor(Sigma.est)

mu.est=c(mean(AnnualGR[,1]),mean(AnnualGR[,2]),
         mean(AnnualGR[,3]),mean(AnnualGR[,4]))

library(MASS)
MVN.AGR=mvrnorm(50,mu.est,Sigma.est)

Fund1.MVN=MVN.AGR[,1]
Fund2.MVN=MVN.AGR[,2]

#Ignore dependencies
Fund1.N=rnorm(50,mean(AnnualGR[,1]),sd(AnnualGR[,1]))
Fund2.N=rnorm(50,mean(AnnualGR[,2]),sd(AnnualGR[,2]))


x11(width=18,height=5)
par(mfrow=c(1,3))
#原始數據
plot(AnnualGR[,1],AnnualGR[,2],type='p',pch=1,lwd=5)
#二元常態抽樣數據（考慮相關性）
plot(Fund1.MVN,Fund2.MVN,type='p',pch=2,lwd=4,col='red')
#常態抽樣
plot(Fund1.N,Fund2.N,type='p',pch=3,lwd=4,col='green')









