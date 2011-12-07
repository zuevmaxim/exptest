epstein.exp.test<-function(x)
{
DNAME <- deparse(substitute(x))
l<-0
n<-length(x)
x<-sort(x)
x<-c(0,x)
D<-(n:1)*(x[2:(n+1)]-x[1:n])
t<-2*n*(log(sum(D)/n)-(sum(log(D)))/n)/(1+(n+1)/(6*n))
p.value<-1-pchisq(t,n-1)
RVAL<-list(statistic=c(EPS=t), p.value=p.value, method="Epstein test for exponentiality",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}