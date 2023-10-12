#-------------------------------------------- --------------------#
#----------------- 2th step calculating Psi.12 vs Psi.3 ---------
#-------------------     calculating  Psi.12  --------------------------
lambda.star
Psi.12 <- matrix(NA,1,length(alpha11))
PSI.12 <- matrix(NA,1,length(alpha11))
for(j in 1:length(alpha11))
{
	Psi.12[1,j]=lambda.star*Psi.1.2[1,j]+(1-lambda.star)*Psi.1.2[2,j]
}
a
#PSI.12_star=lambda.star*Psi.1.2[1,a]+(1-lambda.star)*Psi.1.2[2,a]
PSI.12[1,]=Psi.12[1,]/max(Psi.12)

plot(alpha11, PSI.12,type="l")
#------------------------- Q12 function ------------------------#
Phi.12=matrix(NA,1,length(alpha11))
for(j in 1:length(alpha11))
{
	Phi.12[1,j]=lambda.star*Q.1.2[1,j]+(1-lambda.star)*Q.1.2[2,j]
}
Phi.12

Phi.12[1,aa]
max(Phi.12[1,])

Psi.12=(Phi.12[,aa])/Phi.12[1,]
lines(alpha11,Psi.12,col="blue")
#----------------------  calculating v measure  -----------------#
library(psych); library(base);
like=function(t)
{
	a=t[1]; 	b=t[2];
	for(i in 1:m){
		l=dweibull(x,b,a)* ( (1-pweibull(XR[k,i],b,a))^(R) )
				}
l[l==0]=0.00001
-sum(log(l))     
}

ee=matrix(NA,5,length(alpha11))
for (k in 1:length(alpha11) )
{
	x<-XR[k,(1:m)]
	R<-XR[k,(m+1):(2*m)]

t=c(7.3,.5)              #initial value  for optimation  
max=optim(t,like,method = "BFGS",hessian=TRUE)
parhat=max$par
cov=solve(max$hessian)
Vmeasure= integrate(fn,lower=0,upper=1)
ee[,k]=c(parhat,tr(cov),det(cov),Vmeasure$value) 
}
ee[5,1:501]
plot(alpha11[1:501],ee[5,1:501],type="l")
#----------------------  recalculating Psi3   ----------------
Cv=70; 
Q3<-c()
Psi3<-matrix(NA,1,length(alpha11));
Q3=Cv*ee[5,]
plot(alpha11,Q3)
	Psi3[1,]=min(Q3)/Q3

which(Q3==min(Q3))
XR[11,]
plot(alpha11,Psi3[1,],type="l")
lines(alpha11,PSI.12)
#------------------- calculating new effietioncy------------------#
eta=seq(0,1,.001)
Psi_123 <- matrix(NA,length(alpha11),length(eta))
for(j in 1:length(alpha11))
{
	for(i in 1:length(eta)){
		Psi_123[j,i]=eta[i]*PSI.12[1,j]+(1-eta[i])*Psi3[1,j]
    							}
}
Psi_123
#------------------   finding alpha1 couse most effiency --------#
alpha_opt<-matrix(NA,length(eta),1)
for(j in 1:length(eta) ) 
{
	aa=which(Psi_123[,j]==max(Psi_123[,j]))
	alpha_opt[j,1]=aa[1]
}
alpha_opt
#--------------------  find  Psi_12 & Psi_3 for lambda ------------#
Psi_12.3_eta<-matrix(NA,2,length(eta))
for(i in 1: length(eta)){
	a<-alpha_opt[i,1]
	Psi_12.3_eta[,i]= c(PSI.12[1,a] ,Psi3[1,a] )
}
Psi_12.3_eta
#--------------------  plot of Psi12 & Psi  ---------------------#
xlad<-c()
xlad[2]<-expression(paste(Psi[12](R[lambda][eta])))
xlad[3]<-expression(paste(Psi[3](R[lambda][eta])))

plot(eta,Psi_12.3_eta[1,],lty=2, type="l",ylab="Efficiency",
ylim=c(0,1), xlab=expression(paste(eta)),main="(n=,m=)")
lines(eta,Psi_12.3_eta[2,],lty=1, pch=1, type="l",col="blue")
legend("bottomright", bty="n",xlad[2:3],lty=c(2,1),col=c("black","blue"))
		
	eta.star=0.5
	ss=which(eta==eta.star)
	ss
	aa=alpha_opt[ss]
	alpha11[aa]
    XR[aa,]
xlad[1]<-expression( paste( alpha[1][lambda][eta]^"optim" ) ==-.37 )
legend(locator(1),xlad[1],bty="n")
#-------------------------------------------------
c1=which(Q.1.2[1,]==min(Q.1.2[1,]))
XR[c1,]
alpha11[c1]

c2=which(Q.1.2[2,]==min(Q.1.2[2,]))
XR[c2,]
alpha11[c2]

c3=which( Q3==min(Q3) )
XR[c3,]
alpha11[c3]

aa
lambda.star
eta.star

Q.1.2[1,aa]    # ECrR
Q.1.2[2,aa]    # E(Tm)
Q3[aa]         # V(R)

