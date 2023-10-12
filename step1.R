#-------------------- calculating The sequential approach--------------------#
#----------------- 1th step need RUN generating data-------------------
library(psych); library(base);
m=5; n=10;   h=1000; 
W=matrix(runif(m*h),m,h);

alpha11<-seq(-5,5,.1);  alpha0=-2; lambda=6; beta=.5; Cr=8*(1:m)
length(alpha11)
EEE  <- matrix(NA,4,length(alpha11));
x=matrix(rep(0,m*h),m,h); 
R=matrix(rep(0,m*h),m,h);
P=matrix(rep(0,h*(m-1)),(m-1),h);

for( j in 1:length(alpha11) )
{

	alpha1=alpha11[j]; alpha1;
	for(i in 1:h){
		a<-c(n,m,alpha0,alpha1,lambda,beta)
		data<-dataplogit(a,W[,i])           # here need run function"func-generat data-plogit.R"
		x[,i]<-data$T; 
		R[,i]<-data$r;
		P[,i]<-data$p;
		  		}
	ee=matrix(rep(0,2*h),5,h)
	l1= c();	
	for(k in 1:h)
	{
			like=function(t){
				a=t[1]; 	b=t[2];
					for(i in 1:m){
						l1[i]=dweibull(x[i,k],b,a)* ( (1-pweibull(x[i,k],b,a))^(R[i,k]) )
								}
			l1[l1==0]=0.00001
         	-sum(log(l1))
		              		}
	t=c(6,.5)                
	max=optim(t, like,method = "BFGS",hessian=TRUE)
	parhat=max$par
	cov=solve(max$hessian)
	Vmeasure= integrate(fn,lower=0,upper=1)     # here need run function  "# here need run function.R"
	ee[,k]=c(parhat,tr( cov ), det( cov) , Vmeasure$value) 
	}

	#f=which(	L[1,]<=12 & L[1,]>2 )
	#L=L[,f]
	ER  = sum(Cr*rowMeans(R))
	ETm = mean(x[m,])
	Etrace=mean(ee[2,])
	Evmeasure=mean(ee[5,])
	EEE[,j]=c(ER,ETm,Etrace,Evmeasure)
}
EEE

#write.csv(EEE,file='D:/PHD_thesis/PHD thesis_3th article/Weighted optima design/data/EEEbetado.csv')
#alpha11; length(alpha11); length(EEE[1,]);

#alpha11[485]
#F=which(alpha11==-0.16)
#F=which(EEE[2,]==min(EEE[2,]))
#F=485
#alpha11[F]
#EEE[1,F]
#EEE[2,F]
#EEE[3,F]
#EEE[4,F]


plot(alpha11,EEE[1,])
plot(alpha11,EEE[2,],type="l")
plot(alpha11,EEE[3,])
plot(alpha11,EEE[4,])
#------------------- calculating Q1~ETm & Q2~CrER-----------------------#
Q.1.2.3<- matrix(NA,4,length(alpha11));
C0r=50; C0t=100; C0v=30; Cm=4; Cn=2;   Ct=14;  Cv=23;
Q.1.2.3[1,] = C0r+m*Cm+EEE[1,]
Q.1.2.3[2,] = C0t+n*Cn+Ct*EEE[2,]
Q.1.2.3[3,] = C0v+Cv*EEE[3,]
Q.1.2.3[4,]=Cv*EEE[4,]

par(mfrow=c(1,4 ))
plot(alpha11,Q.1.2.3[1,],type="l")
plot(alpha11,Q.1.2.3[2,],type="l",col="blue")
plot(alpha11,Q.1.2.3[3,],type="l",col="red")
plot(alpha11,Q.1.2.3[4,],type="l",col="pink")
#------------------------ calculating Psi1 & Psi2-----------------------#
Psi.1.2.3<-matrix(NA,4,length(alpha11));
	Psi.1.2.3[1,]=min(Q.1.2.3[1,])/Q.1.2.3[1,]
	Psi.1.2.3[2,]=min(Q.1.2.3[2,])/Q.1.2.3[2,]
	Psi.1.2.3[3,]=min(Q.1.2.3[3,])/Q.1.2.3[3,]
	Psi.1.2.3[4,]=min(Q.1.2.3[4,])/Q.1.2.3[4,]

Psi.1.2.3
plot(alpha11,Psi.1.2.3[1,],type="l",ylim=c(0,1))
lines(alpha11,Psi.1.2.3[2,],col="blue")
lines(alpha11,Psi.1.2.3[3,],col="red")
lines(alpha11,Psi.1.2.3[4,],col="pink")
#------------------------- calculating effietioncy----------------------#
lambda=seq(0,1,.01)
Psi <- matrix(NA,length(alpha11),length(lambda))
for(j in 1:length(alpha11))
{
	for(i in 1:length(lambda)){
		Psi[j,i]=lambda[i]*Psi.1.2.3[1,j]+(1-lambda[i])*Psi.1.2.3[2,j]
    							}
}
Psi
#--------------------   finding alpha1 couse most effiency -------------#
alphahat<-matrix(NA,length(lambda),1)
for(j in 1:length(lambda) ) 
{
	a=which(Psi[,j]==max(Psi[,j]))
	alphahat[j,1]=a[1]
}
alphahat
#-----------------------  find  Psi1 & Psi2 for lambda -----------------#
Psi.1.2.3_lambda<-matrix(NA,2,length(lambda))
for(i in 1: length(lambda)){

	a<-alphahat[i,1]
	Psi.1.2.3_lambda[,i]= c(Psi.1.2.3[1,a] ,Psi.1.2.3[2,a] )
}
Psi.1.2.3_lambda

#------------------------  plot of Psi1 & Psi2  ------------------------#
xlad<-c()
xlad[2]<-expression(paste(Psi[1](R[lambda])))
xlad[3]<-expression(paste(Psi[2](R[lambda])))

plot(lambda,Psi.1.2.3_lambda[1,],lty=2, type="l",ylab="Efficiency",
ylim=c(0,1), xlab=expression(paste(lambda)),main="(n=10,m=5)")

lines(lambda,Psi.1.2.3_lambda[2,],lty=1, pch=1, type="l",col="blue")
legend("bottomright", bty="n",xlad[2:3],lty=c(2,1),col=c("black","blue"))
		
	lambda.star = 0.68
	s=which(lambda==(lambda.star))
s
	#f=which(Psi[,s]==min(Psi[,s]))
	#Psi[f,s]
	s
	a=alphahat[s]
	alpha11[a]
xlad[1]<-expression(paste(alpha[1][lambda]^"*")==-0.9)
legend(locator(1),xlad[1],bty="n")
#-----------------------------------------------------------------------
lambda[69]; alphahat[s]; alpha11[a];
# information of efficiency point
a
alpha11[a]
lambda.star
Psi.1.2.3[1,a]
Psi.1.2.3[2,a]
PSI.12_star=lambda.star*Psi.1.2.3[1,a]+(1-lambda.star)*Psi.1.2.3[2,a]
PSI.12_star



#--------- comparing single objective with compouned--------------------#
#------------------  table 1 in article  ----------------------------
k1=which(Q.1.2.3[1,]==min(Q.1.2.3[1,]))
alpha11[k1]
ECrR_Q1=min(Q.1.2.3[1,k1])-C0r-m*Cm
ETm_Q1= Q.1.2.3[2,k1] - C0t-n*Cn
V_Q1= Q.1.2.3[4,k1]/ Cv
ETm_Q1; ECrR_Q1;  V_Q1;

k2=which(Q.1.2.3[2,]==min(Q.1.2.3[2,]))
alpha11[k2]
ECrR_Q2=min(Q.1.2.3[1,k2])-C0r-m*Cm
ETm_Q2= Q.1.2.3[2,k2] - C0t-n*Cn
V_Q2= Q.1.2.3[4,k2]/ Cv
ETm_Q2; ECrR_Q2;  V_Q2;

k3=a
ECrR_Q3=min(Q.1.2.3[1,k3])-C0r-m*Cm
ETm_Q3= Q.1.2.3[2,k3] - C0t-n*Cn
V_Q3= Q.1.2.3[4,k3]/ Cv
ETm_Q3; ECrR_Q3;  V_Q3;

#----------------------------------------------------------------------

