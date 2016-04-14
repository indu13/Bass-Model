bass<-read.csv("bass_file1.csv",head=TRUE)
head(bass)

bass
sales=ts(bass$Sales, start=c(2004))
plot(sales, type="l",lty=2,col="red")
points(sales,col="blue")

Y=cumsum(sales)

(Y=ts(Y,start=c(2004,1),freq=1))

plot(Y,type="l",lty=2,col="red")
points(Y,pch=20,col="blue")
title("Sales")

length(Y)

(Y=c(0,Y[1:(length(Y)-1)]))
(Ysq=Y^2)
(out=lm(Sales~Y+Ysq,data=bi))
summary(out)


a=out$coef[1]
b=out$coef[2]
c=out$coef[3]


(mplus=(-b+sqrt(b^2-4*a*c))/(2*c))
(mminus=(-b-sqrt(b^2-4*a*c))/(2*c))

(m=mminus)
(p=a/m)
(q=b+p)


plot()


Ext=NULL

for(t in 1:length(Y)){
  Ext<-c(Ext,p*(m-Y[t]))
}

plot(Ext,type='l')


Int=NULL

for(t in 1:length(Y)){
  Int<-c(Int,q*(Y[t]/m)*(m-Y[t]))
}

plot(Int,type='l')


Bass_Model=function(p,q,m,T=14){
  S=double(T)
  Y=double(T+1)
  Y[1]=0
  for(t in 1:T){
    S[t]=p*m+(q-p)*Y[t]-(q/m)*Y[t]^2
    Y[t+1]=Y[t]+S[t]
    
  }
  
  return(list(sales=S, cumSales=cumsum(S)))
}


sales
spred
spred=Bass_Model(p,q,m,T=13)$sales
spred=ts(spred,start=c(2004,1),frequency=1)
ts.plot(sales, spred, col=c("blue","red"))
legend("topleft",legend=c("actual","Bass Model"), fill=c("blue","red"))

spred=Bass_Model(p,q,m)$cumSales
Cumspred=ts(cumsum(spred),start=2004)
CumSales=ts(cumsum(Sales),start=2004)
ts.plot(CumSales, Cumspred, col=c("blue","red"))
legend("topleft",legend=c("actual","Bass Model"), fill=c("blue","red"))
