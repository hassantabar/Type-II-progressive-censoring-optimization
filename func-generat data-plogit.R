rm(list=ls(all=TRUE))
###-------------------- Generate data from plogit model by 2th algorithm--------------###
dataplogit<-function(a,W)
{
	n=a[1]; m=a[2]; alpha0=a[3]; alpha1=a[4]; 
	lambda=a[5]; beta=a[6];
  	W=W

	V<-c(); U<-c(); T<-c(); p<-c(); r<-c();
	V[1]= W[1]^(1/n)
	U[1]= 1-V[1]
	T[1]= lambda*( -log(1-U[1]) )^(1/beta)
	p[1]= exp(alpha0+alpha1*T[1])/(1+exp(alpha0+alpha1*T[1]))
	r[1]= rbinom(1,n-m,p[1])
	i=2
	while(i<=m-1)
	{
		V[i]=W[i]^( 1/(n-i+1-sum(r)) )
		U[i]=1-prod(V[1:i])
		T[i]=lambda*( -log(1-U[i]) )^(1/beta)
		p[i]=exp(alpha0+alpha1*(T[i]-T[i-1]))/(1+exp(alpha0+alpha1*(T[i]-T[i-1])))

			if(n-m-sum(r)>0){	
				r[i]= rbinom(1 ,n-m-sum(r) , prob=p[i])
			} else{
			r[i]=0
			}
		i=i+1
	}

	V[m]=W[m]^( 1/(n-m+1-sum(r)) )
	U[m]=1-prod(V[1:m])
	T[m]=lambda*( -log(1-U[m]) )^(1/beta)
	r[m]=n-m-sum(r)
	l<-list(T,r,p)
	names(l)<-c("T","r","p")
return(l)
}
#------------------------- example for one generation-------------------------
par(mfrow=c(2,2 ))
m=6;
W<-runif(m,0,1)

n=12; alpha0=3;  lambda=5; beta=2; 
alpha1=-1;
a<-c(n,m,alpha0,alpha1,lambda,beta,p)

X<-dataplogit(a,W)
X
c(X$T[1],diff(X$T))

rweibull(W, shape=beta, scale = lambda)

Tprim=c(X$T[1],diff(X$T))
P=X$p
var(X$T)
hist(X$T,main="",ylab=)
hist(Tprim,xlab="Failure Distance")

plot(Tprim[1:(m-1)],P,lty=1,ylim=c(0,1),xlim=Failure distance)
lines(Tprim[1:(m-1)],P,lty=2)