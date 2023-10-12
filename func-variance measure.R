#------------------     variance measure  function  -------------------#

fn<-function(p)
{
		#Tp = c( 1/max$par[1] , -log(-log(1-p))/(max$par[2])^2 )
		#vTp= (Tp %*% cov ) %*%  t( t(Tp) )
	g=log(-log(1-p))
     v=(1/max$par[1]^2)*cov[1,1]-cov[1,2]*g/(max$par[1]*max$par[2])+
 ( -cov[1,2]/max$par[1]+cov[2,2]*g /max$par[2]^2 )*g

}

integrate(fn,lower=0,upper=1)


