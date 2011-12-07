gnedenko.exp.test<-function(x, R=length(x)/2)
{
DNAME <- deparse(substitute(x))
R<-round(R)
n<-length(x)
x<-sort(x)
x<-c(0,x)
D<-(n:1)*(x[2:(n+1)]-x[1:n])
t<-(sum(D[1:R])/R)/(sum(D[(R+1):n])/(n-R))
p.value<-2*min(pf(t,2*R,2*(n-R)),1-pf(t,2*R,2*(n-R)))
RVAL<-list(statistic=c(Q=t), p.value=p.value, method="Gnedenko's F-test of exponentiality",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}