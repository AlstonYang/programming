#Q1
#(a)
odd=which(c(1:100)%%2 !=0)

#(b)
mean(odd)
median(odd)
min(odd)
max(odd)
range(odd)

#(c)
sd(odd)
quantile(odd,0.05)
quantile(odd,0.95)

#(d)
even = which(c(1:100)%%2 ==0)

#(e)
if(mean(odd)>mean(even)){
  print('odd has the higher mean')
}else{
  print('even has the higher mean')
}

if(var(odd)>var(even)){
  print('odd has the higher variance')
}else{
  print('even has the higher variance')
}

#Q2
#(a)
rep(0:4,each=5)
#(b)
rep(seq(1,5,1),5)

#Q3
#(a)
vector=c(1:100)
vector=vector[which(vector%%2!= 0 & vector%%3!= 0 & vector%%7 != 0)]

#(b)
identity = diag(nrow = 10,ncol = 10)
diag(nrow = 10,ncol = 10)*5
identity[which(identity==1)] = identity[which(identity==1)]*5

#Q4
#(a)
factorial(4)
factorial(50)
factorial(5000)

#(b)
choose(4,2)
choose(50,20)
choose(5000,2000)

#(c)
e.value=function(j){
  log=0
  for (i in 1:j) {
    log=log+log(i,exp(1))
  }
  return(log)
}

e.value(5000)-(e.value(2000)+e.value(3000))


#5
#(a)
f=expression(x^4+2*x^2+exp(1)^x)
g=D(f,"x") 
gfun=function(x){eval(g)}

sg=D(D(f,"x"),"x") 
sgfun=function(x){eval(g)}

#(b)
x0=1
gfun(x0)

#Q6
install.packages('cubature')
library(cubature)
f=function(x){(sqrt(1-(abs(x)-1)^2))-(acos(1-abs(x))-pi)}
adaptIntegrate(f, lowerLimit = -2,upperLimit =  2)

#Q7
#(a)
data(Orange)
Orange
org=plot(x=Orange$circumference,y=log(Orange$age),ylim = c(4,8))

cor(Orange$circumference, log(Orange$age))

#(b)
error1=function(w){
  
  w1=w[1]
  w2=w[2]
  
  y=sum((log(Orange$age)-(w1*Orange$circumference)/(w2+Orange$circumference))^2)
  return(y)
}

error1(c(0.5,0.5))

#(c)

optim(c(0.5,0.5), error1)

#(d)
x=min(Orange$circumference):max(Orange$circumference)

lines(x=x,y=(8.06170*x)/(19.71464+x),type="l",col="green")

#(e)
error2=function(b){
  
  b0=b[1]
  b1=b[2]
  b2=b[3]
  
  y=sum((log(Orange$age)-(b0+(b1*Orange$circumference)+(b2*Orange$circumference^2)))^2)
  return(y)
}

error2(c(2,0.5,-0.1))


#(f)
optim(c(2,0.5,-0.1), error2)

#(g)
lines(x=x,y=(1.7538235593+(0.0767490356*x)+(-0.0002525347*x^2)),type="l",col="red")

#(h)
mean.distan=function(x0,y0,x1,y1){
  
  x0n=length(x0)
  x1n=length(x1)
  distance = 0
  for(i in 1:x0n){
    for(j in 1:x1n){
      if(x0[i]==x1[j]){
        distance=sqrt((y0[i]-y1[j])^2)+distance
      }
    }
  }
    return(distance/x0n)

}

x0cord=c(Orange$circumference)
y0cord=c(log(Orange$age))

x1cord=c(x)
y1cord=c((8.06170*x)/(19.71464+x))
mean.distan(x0cord,y0cord,x1cord,y1cord)

y2cord=c(1.7538235593+(0.0767490356*x)+(-0.0002525347*x^2))
mean.distan(x0cord,y0cord,x1cord,y2cord)





