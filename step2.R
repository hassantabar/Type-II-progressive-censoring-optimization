#-------------------------------------------- --------------------------#
#----------------- 2th step calculating Psi.12 vs Psi.3 ----------------#
#-------  calculating  Psi.12 based on Psi -----------------------------
lambda.star
Psi.12 <- matrix(NA,1,length(alpha11))
PSI.12 <- matrix(NA,1,length(alpha11))
for(j in 1:length(alpha11))
{
	Psi.12[1,j]=lambda.star*Psi.1.2.3[1,j]+(1-lambda.star)*Psi.1.2.3[2,j]
}
a
PSI.12_star=lambda.star*Psi.1.2.3[1,a]+(1-lambda.star)*Psi.1.2.3[2,a]
PSI.12[1,]=Psi.12[1,]/max(Psi.12)
plot(alpha11,PSI.12,col="red")
#------------------------------------------------------------------------
#-------  calculating  Psi.12 based on phi(Q)---------------------------#
Phi.12=matrix(NA,1,length(alpha11))
for(j in 1:length(alpha11))
{
	Phi.12[1,j]=lambda.star*Q.1.2.3[1,j]+(1-lambda.star)*Q.1.2.3[2,j]
}
Phi.12
plot(alpha11,Phi.12)
plot(alpha11,Q.1.2.3[4,])

which(Phi.12[1,]==min(Phi.12[1,]))
alpha11
Phi.12_star=lambda.star*Q.1.2.3[1,a]+(1-lambda.star)*Q.1.2.3[2,a]
PSI.12[1,]=min(Phi.12[1,])/Phi.12[1,]    # a the palce that happen optimal design of pervious step
plot(alpha11, PSI.12)
#----------------------  recalculating Psi3   --------------------------#
	Psi.1.2.3[3,]=min(Q.1.2.3[3,])/Q.1.2.3[3,]
	Psi.1.2.3[4,]=min(Q.1.2.3[4,])/Q.1.2.3[4,]

Psi.1.2.3[4,]
plot(alpha11,PSI.12[1,])
lines(alpha11,Psi.1.2.3[4,],,type="l")
#------------------- calculating new effietioncy------------------------#
eta=seq(0,1,.05)
Psi_123 <- matrix(NA,length(alpha11),length(eta))
for(j in 1:length(alpha11))
{
	for(i in 1:length(eta)){
		Psi_123[j,i]=eta[i]*PSI.12[1,j]+(1-eta[i])*Psi.1.2.3[4,j]
    							}
}
Psi_123
#------------------   finding alpha1 couse most effiency ---------------#
alpha_opt<-matrix(NA,length(eta),1)
for(j in 1:length(eta) ) 
{
	aa=which(Psi_123[,j]==max(Psi_123[,j]))
	alpha_opt[j,1]=aa[1]
}
alpha_opt
#--------------------  find  Psi_12 & Psi_3 for lambda -----------------#
Psi_12.3_eta<-matrix(NA,2,length(eta))
for(i in 1: length(eta)){
	a<-alpha_opt[i,1]
	Psi_12.3_eta[,i]= c(PSI.12[1,a] ,Psi.1.2.3[4,a] )
}
Psi_12.3_eta
#-----------------------  plot of Psi1 & Psi2  -------------------------#
xlad<-c()
xlad[2]<-expression(paste(Psi[12](R[lambda][eta])))
xlad[3]<-expression(paste(Psi[3](R[lambda][eta])))

plot(eta,Psi_12.3_eta[1,],lty=2, type="l",ylab="Efficiency",
ylim=c(0.8,1), xlab=expression(paste(eta)),main="(n=10,m=5)")
lines(eta,Psi_12.3_eta[2,],lty=1, pch=1, type="l",col="blue")
legend("bottomright", bty="n",xlad[2:3],lty=c(2,1),col=c("black","blue"))
		
	eta.star=0.25
	ss=which(eta==eta.star)
	ss
	aa=alpha_opt[ss]
	alpha11[aa]
xlad[1]<-expression( paste( alpha[1][lambda][eta]^"optim" ) ==0.3 )
legend(locator(1),xlad[1],bty="n")
#-----------------------comparing single objective with sequential------------#
a
lambda.star
Psi.1.2.3[1,a]
Psi.1.2.3[2,a]
PSI.12_star=lambda.star*Psi.1.2.3[1,a]+(1-lambda.star)*Psi.1.2.3[2,a]
PSI.12_star

ss
eta.star
Psi.123_star=eta.star*PSI.12[1,aa]+(1-eta.star)*Psi.1.2.3[4,aa]
Psi.123_star
PSI.12[1,aa]
Psi.1.2.3[1,aa]
Psi.1.2.3[2,aa]
Psi.1.2.3[4,aa]

#______________________________________________________________________________
#----------- for step equvalency of this method and e-constrain---
C_M=min(Q.1.2.3[1,])/ Psi.1.2.3[1,ss]
C_N=min(Q.1.2.3[2,])/ Psi.1.2.3[2,ss]
C_M; C_N;

