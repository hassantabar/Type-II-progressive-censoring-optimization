#write.csv(XR, file = 'C:/Users/ASUS/Desktop/data/XR.csv')

#XR <- read.csv(file = 'C:/Users/ASUS/Desktop/data/XR.csv')
#XR <- as.matrix(XR)
#XR
#-------------------------   nelson's data  ----------------------------#  
##X<-c( -1.6608, -2.2485, -.0409, .2700, 1.0224, 1.1505, 1.4231, 1.5411, 1.5789, 1.8718
, 1.9947, 2.0806, 2.1126, 2.4898, 3.4578, 3.4818, 3.5237, 3.6030, 4.2889)

X<- c(0.19 , 0.78, 0.96, 1.31, 2.78, 3.16, 4.15, 4.67, 4.85, 6.50,
7.35, 8.0, 8.27, 12.06, 31.75, 32.52, 33.91, 36.71, 72.89)
length(X)
hist(X)
mean(X)
#----------------------- generating data (7-stage algorithm)------------#
m=8; n=19;   h=10000; 
W=matrix(runif(m*h),m,h);
alpha11<-seq(-15,15,.2); alpha0=-.5; gamma=7.18; beta=.5;
length(alpha11)

XR  <- matrix(NA,length(alpha11), 2*m);
x=matrix(rep(0,m*h),m,h); 
R=matrix(rep(0,m*h),m,h);
P=matrix(rep(0,h*(m-1)),(m-1),h);

for(j in 1:length(alpha11))
{
	alpha1=alpha11[j]; alpha1;
	for(i in 1:h){
		a<-c(n,m,alpha0,alpha1,gamma,beta)
		data<-dataplogit(a,W[,i])
		x[,i]<-data$T; 
		R[,i]<-data$r;
		P[,i]<-data$p;
		  		}  
	k=which(x[1,]>0.1845 & x[1,]<0.194 )
		cv<-c()
		xnew<-matrix(NA,m,length(k))
			for(i in 1:length(k) ){
				#j=k[i]
				cv[i]=sqrt(var(x[,k[i]]))/ mean(x[,k[i]])
     			xnew[,i]<- x[,k[i]]
								}
		cv
		xnew
		Ti_mean=rowMeans(xnew)
         cv_t= sqrt(var(Ti_mean))/ mean(Ti_mean)

	a=which(  abs(cv-cv_t)== min( abs(cv-cv_t) )	)
	x[,k[a]]
	R[,k[a]]
	XR[j, ] <- c(x[,k[a]], R[,k[a]] ) 
}
XR

#----------------  calculating 2 critria ------------------------#
Q.1.2 <- matrix(NA,2, length(alpha11))
C0r=50; C0t=100; C0v=30; Cm=4; Cn=2;   Ct=12;  Cr=8*(1:m)
Q.1.2[1,] = C0r+m*Cm+ rowSums(Cr*XR[,(m+1): (2*m) ])
Q.1.2[2,] = C0t+n*Cn+Ct*XR[,m]

Q.1.2
plot(alpha11,Q.1.2[1,],type="l")
plot(alpha11,Q.1.2[2,],type="l")
#-------------------- calculating Psi1 & Psi2--------------------#
Psi.1.2<- matrix(NA,2,length(alpha11));
		Psi.1.2[1,]=min(Q.1.2[1,])/Q.1.2[1,]
		Psi.1.2[2,]=min(Q.1.2[2,])/Q.1.2[2,]

Psi.1.2
plot(alpha11,Psi.1.2[1,],type="l")
lines(alpha11,Psi.1.2[2,],type="l",col="blue")
#------------------- calculating effietioncy--------------------#
lambda=seq(0,1,.001)
Psi<-matrix(NA,length(alpha11),length(lambda))
for(j in 1:length(alpha11))
{
	for(i in 1:length(lambda)){
		Psi[j,i]=lambda[i]*Psi.1.2[1,j]+(1-lambda[i])*Psi.1.2[2,j]
    								}
}
Psi.1.2
Psi

#------------------   finding alpha1 couse most effiency --------#
alphahat<-matrix(NA,length(lambda),1)
for(j in 1:length(lambda) ) 
{
	a=which(Psi[,j]==max(Psi[,j]))
	alphahat[j,1]=a[1]
}
alphahat
#--------------------  find  Psi1 & Psi2 for lambda ------------#
Psi.1.2_lambda<-matrix(NA,2,length(lambda))
for(i in 1: length(lambda)){

	a<-alphahat[i,1]
	Psi.1.2_lambda[,i]= c(Psi.1.2[1,a] ,Psi.1.2[2,a] )
}
Psi.1.2_lambda
#--------------------  plot of Psi1 & Psi2  ---------------------#
xlad<-c()
xlad[2]<-expression(paste(Psi[1](R[lambda])))
xlad[3]<-expression(paste(Psi[2](R[lambda])))

plot(lambda,Psi.1.2_lambda[1,],lty=2, type="l",ylab="Efficiency",
ylim=c(0,1), xlab=expression(paste(lambda)),main="(n=19,m=8)")

lines(lambda,Psi.1.2_lambda[2,],lty=1, pch=1, type="l",col="blue")

legend("bottomright", bty="n",xlad[2:3],lty=c(2,1),col=c("black","blue"))
		
	lambda.star=0.222
	s=which(lambda==lambda.star); 	s;
	aa= alphahat[s,]
	alpha11[aa]
    XR[aa,]	
xlad[1]<-expression(paste(alpha[1][lambda]^"*")==0.222)
xlad[4]<-expression(paste(R[lambda]^"*")==c(1,5,1,4,0,0,0,0))
legend(locator(1),xlad[4],bty="n")
