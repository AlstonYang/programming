#Q2
set.seed(5566)
cost=10
S=10000

#before the game 
P.pre = 25
D.pre.mu = 9000
D.pre.sig = 2000
D.pre = round(rnorm(S, D.pre.mu, D.pre.sig),0)

#the simulation of bowl game
Prob.win = 0.4
#1 symbolize success, otherwise is 0.
Result.game = sample(c(1,0),S,prob=c(Prob.win,1-Prob.win),replace = TRUE)

#after the game -win
P.win = 25
D.win.mu=6000
D.win.sig=2000
D.win = round(rnorm(S,D.win.mu,D.win.sig),0)

#after the game -loss
P.loss = 12.5
D.loss.mu = 2000
D.loss.sig = 100
scale = (D.loss.sig^2)/D.loss.mu
shape = D.loss.mu/scale
D.loss = round(rgamma(S, shape = shape, scale = scale),0)

Q = seq(3000,15000,100)

PF = c()
EPF = c()
CVaR =c()
# EPF = matrix(NA,ncol=length(Q))
# colnames(EPF)=Q
# CVaR = matrix(NA,ncol=length(Q))
# colnames(CVaR)=Q

for (q in 1:length(Q)){
  for (i in 1:S){
    if (Result.game[i]==1){
      PF[i] = (P.pre-cost)*min(Q[q],(D.pre[i]+D.win[i]))-cost*max((Q[q]-D.pre[i]-D.win[i]),0)
    }else if (Result.game[i]==0){
      PF[i] = (P.pre-cost)*min(Q[q],D.pre[i])+(P.loss-cost)*max(min((Q[q]-D.pre[i]),D.loss[i]),0)-cost*max((Q[q]-D.pre[i]-D.loss[i]),0)
    }
  }
  EPF[q] = mean(PF)
  CVaR[q] = mean(PF[which(PF<=quantile(PF,0.1))])
}

x11(width=18,height=5)
par(mfrow=c(1,2))

#(1)
plot(Q, EPF,type = 'l',xaxt="n",yaxt="n",lwd=3,xlab="Production quantity",ylab="EPF")
points(Q[which.max(EPF)],max(EPF),pch=8,col='red')
axis(1,seq(3000,15000,1000))
y_axis = round(seq(from=round(min(EPF),0), to=round(max(EPF,1000),0), length.out=5),0)
axis(2,y_axis)
text(Q[which.max(EPF)]+1500,max(EPF),labels = "(12600, 144890.5)",cex=1,col='red')

#(2)
plot(Q,CVaR,type = 'l',xaxt="n",yaxt="n",lwd=3,xlab="Production quantity",ylab="CVAR(q=10%)")
points(Q[which.max(CVaR)],max(CVaR),pch=8,col='red')
axis(1,seq(3000,15000,1000))
y_axis = round(seq(from=round(min(CVaR),0), to=round(max(CVaR),0), length.out=5),0)
axis(2,y_axis)
text(Q[which.max(CVaR)]+1500,max(CVaR),labels = "(6900, 102058.8)", cex=1, col='red')

#(3) 
cu.pre = P.pre-cost
co.pre = cost
frac.pre = cu.pre / (co.pre + cu.pre)
Q.win = round(quantile(D.pre,frac.pre,names=FALSE)+
                quantile(D.win,frac.pre,names = FALSE),0)


cu.loss = P.loss-cost
co.loss = cost
frac.loss = cu.loss / (co.loss + cu.loss)
Q.loss = round(quantile(D.pre,frac.pre,names=FALSE)+
                 quantile(D.loss,frac.loss,names = FALSE),0)

PF.perfectinfo = c()
for (i in 1:S){
  if (Result.game[i]==1){
    PF.perfectinfo[i] = (P.pre-cost)*min(Q.win,(D.pre[i]+D.win[i]))-cost*max((Q.win-D.pre[i]-D.win[i]),0)
  }else if (Result.game[i]==0){
    PF.perfectinfo[i] = (P.pre-cost)*min(Q.loss,D.pre[i])+(P.loss-cost)*max(min((Q.loss-D.pre[i]),D.loss[i]),0)-cost*max((Q.loss-D.pre[i]-D.loss[i]),0)
  }
}

mean(PF.perfectinfo)-max(EPF)
# Result.game[1:10]
# summary(PF.perfectinfo)
# summary(EPF)
# x=seq(round(min(D.loss),0), round(max(D.loss),0), 1)
# hist(D.loss ,breaks = 100,freq=FALSE)
# lines(x, dgamma(x,shape=shape,scale=scale),col='red',lwd=3,type = 'l')

